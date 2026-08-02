# Tests for pubertal/Tanner-stage BDS numbers (313/315/317/825/312) and
# their varName sideload fallback (gen/phb/bre/phg/men/tv).

make_json <- function(client_details, client_measurements) {
  json_data <- list(
    Format = "3.1",
    clientDetails = client_details,
    clientMeasurements = client_measurements
  )
  jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
}

boy <- list(
  list(bdsNumber = 19, value = "1"),
  list(bdsNumber = 20, value = "20100101")
)
girl <- list(
  list(bdsNumber = 19, value = "2"),
  list(bdsNumber = 20, value = "20100101")
)

# -- bdsNumber-only path --------------------------------------------------

test_that("bdsNumber 313/315 (boy) resolve to yname gen/phb with no z-score", {
  js <- make_json(boy, list(
    list(bdsNumber = 313, values = list(list(date = "20240315", value = "03"))),
    list(bdsNumber = 315, values = list(list(date = "20240315", value = "02")))
  ))
  tgt <- read_bds(js, format = "3.1")
  xyz <- tgt$xyz

  expect_setequal(xyz$yname, c("gen", "phb"))
  expect_equal(xyz$y[xyz$yname == "gen"], 3)
  expect_equal(xyz$y[xyz$yname == "phb"], 2)
  expect_true(all(is.na(xyz$z)))
  expect_true(all(is.na(xyz$zref)))
  expect_true(all(is.na(xyz$zname)))
})

test_that("bdsNumber 317/825 (girl) resolve to yname bre/phg with no z-score", {
  js <- make_json(girl, list(
    list(bdsNumber = 317, values = list(list(date = "20240315", value = "04"))),
    list(bdsNumber = 825, values = list(list(date = "20240315", value = "03")))
  ))
  tgt <- read_bds(js, format = "3.1")
  xyz <- tgt$xyz

  expect_setequal(xyz$yname, c("bre", "phg"))
  expect_equal(xyz$y[xyz$yname == "bre"], 4)
  expect_equal(xyz$y[xyz$yname == "phg"], 3)
  expect_true(all(is.na(xyz$z)))
})

test_that("BDS P6 (stage 06) for phb/phg is recoded to stage 5", {
  js <- make_json(girl, list(
    list(bdsNumber = 825, values = list(list(date = "20240315", value = "06")))
  ))
  tgt <- read_bds(js, format = "3.1")
  xyz <- tgt$xyz

  expect_equal(xyz$y[xyz$yname == "phg"], 5)
})

test_that("BDS 312 (Datum menarche) is stored as psn$mendate, not xyz", {
  js <- make_json(girl, list(
    list(bdsNumber = 313, values = list(list(date = "20240501", value = "01"))),
    list(bdsNumber = 312, values = list(list(date = "20240501", value = "20240310")))
  ))
  tgt <- read_bds(js, format = "3.1")

  expect_equal(as.character(tgt$psn$mendate), "2024-03-10")
  expect_false("men" %in% tgt$xyz$yname)
  expect_false("mendate" %in% tgt$xyz$yname)
})

test_that("BDS 312 value is not confused with its own visit date", {
  # visit date (20240501) intentionally differs from the menarche date
  # (20240310) itself, to guard against check_ranges_3() collapsing the
  # measurement value into the visit-date column
  js <- make_json(girl, list(
    list(bdsNumber = 312, values = list(list(date = "20240501", value = "20240310")))
  ))
  tgt <- read_bds(js, format = "3.1")

  expect_equal(as.character(tgt$psn$mendate), "2024-03-10")
})

# -- varName sideload path -------------------------------------------------

test_that("varName sideload works for all pubertal types, including tv and men", {
  js <- make_json(boy, list(
    list(varName = "gen", values = list(list(date = "20240315", value = "3"))),
    list(varName = "phb", values = list(list(date = "20240315", value = "2"))),
    list(varName = "tv", values = list(list(date = "20240315", value = "8"))),
    list(varName = "men", values = list(list(date = "20240315", value = "1")))
  ))
  tgt <- read_bds(js, format = "3.1")
  xyz <- tgt$xyz

  expect_setequal(xyz$yname, c("gen", "phb", "tv", "men"))
  expect_equal(xyz$y[xyz$yname == "gen"], 3)
  expect_equal(xyz$y[xyz$yname == "tv"], 8)
  expect_true(all(is.na(xyz$z[xyz$yname %in% c("gen", "phb", "tv", "men")])))
})

test_that("varName sideload P6 (stage 6) for phb/phg is also recoded to 5", {
  js <- make_json(boy, list(
    list(varName = "phb", values = list(list(date = "20240315", value = "6")))
  ))
  tgt <- read_bds(js, format = "3.1")

  expect_equal(tgt$xyz$y[tgt$xyz$yname == "phb"], 5)
})

# -- mixed bdsNumber + varName, same record --------------------------------

test_that("bdsNumber and varName paths coexist for different visits/ynames", {
  js <- make_json(boy, list(
    list(bdsNumber = 313, values = list(list(date = "20240315", value = "03"))),
    list(varName = "tv", values = list(list(date = "20240315", value = "8"))),
    list(varName = "phb", values = list(list(date = "20240501", value = "4")))
  ))
  tgt <- read_bds(js, format = "3.1")
  xyz <- tgt$xyz

  expect_equal(nrow(xyz), 3)
  expect_equal(xyz$y[xyz$yname == "gen"], 3)
  expect_equal(xyz$y[xyz$yname == "tv"], 8)
  expect_equal(xyz$y[xyz$yname == "phb" & xyz$age > 14], 4)
})

test_that("on (age, yname) conflict, the bdsNumber-derived row wins over sideload", {
  js <- make_json(boy, list(
    list(bdsNumber = 313, values = list(list(date = "20240315", value = "03"))),
    list(varName = "gen", values = list(list(date = "20240315", value = "1")))
  ))
  tgt <- read_bds(js, format = "3.1")
  xyz <- tgt$xyz

  # exactly one gen row for this age, and it's the bdsNumber-derived value
  gen_rows <- xyz[xyz$yname == "gen", ]
  expect_equal(nrow(gen_rows), 1)
  expect_equal(gen_rows$y, 3)
})

# -- regression: existing bdsNumber-only files are unaffected ---------------

test_that("existing v3.0 fixtures are unaffected by pubertal BDS changes", {
  data2 <- system.file("extdata", "bds_v3.0", "smocc", "Laura_S.json",
                        package = "jamesdemodata")
  q <- read_bds(data2, format = "3.0")
  expect_true(nrow(q$xyz) > 0)
  expect_false(any(q$xyz$yname %in% c("gen", "phb", "bre", "phg", "men", "tv")))
})
