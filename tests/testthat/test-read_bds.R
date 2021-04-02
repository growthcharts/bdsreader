# test the empty object
js1 <- '{"OrganisatieCode":0,"ClientGegevens":{}}'
test_that("handles the empty individual object", {
  expect_message(
    read_bds(js1),
    "should have required property 'Elementen'"
  )
})

jtf <- system.file("extdata", "test", paste0("test", 1:25, ".json"), package = "jamestest")

test_that("test1.json (client3.json) passes read_bds()", {
  expect_equal(
    class(read_bds(jtf[1])),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("test2.json (missing Referentie) PASSES", {
  expect_equal(
    class(read_bds(jtf[2])),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("test3.json (missing OrganisatieCode) MESS", {
  expect_message(
    read_bds(jtf[3]),
    "should have required property 'OrganisatieCode'"
  )
})

test_that("test4.json (wrong type) MESS", {
  expect_message(
    read_bds(jtf[4]),
    ".OrganisatieCode should be integer"
  )
})

test_that("test5.json (missing ClientGegevens) MESS", {
  expect_message(
    read_bds(jtf[5]),
    "should have required property 'ClientGegevens'"
  )
})

test_that("test6.json (Missing ContactMomenten) MESS", {
  expect_message(read_bds(jtf[6]),
    "Missing 'Contactmomenten'",
    fixed = TRUE
  )
})

test_that("test7.json (Missing Referentie & OrganisatieCode) MESS", {
  expect_message(
    read_bds(jtf[7]),
    "should have required property 'OrganisatieCode'"
  )
})

test_that("test8.json (Invalid JSON) ERROR", {
  expect_error(
    read_bds(jtf[8]),
    "lexical error: invalid char in json text."
  )
})

test_that("test9.json (Bdsnummer 19 missing) MESS", {
  expect_message(
    read_bds(jtf[9]),
    "verplicht BDS nummer ontbreekt: 19"
  )
})

test_that("test10.json (Bdsnummer 20 missing) MESS", {
  expect_message(
    read_bds(jtf[10]),
    "verplicht BDS nummer ontbreekt: 20"
  )
})

test_that("test11.json (Bdsnummer 82 missing) MESS", {
  expect_message(read_bds(jtf[11]),
    "BDS 82 (Zwangerschapsduur in dagen) heeft geen waarde",
    fixed = TRUE
  )
})

test_that("test12.json (Bdsnummer 91 missing) PASSES", {
  expect_silent(read_bds(jtf[12]))
})

test_that("test13.json (Bdsnummer 110 missing) MESS", {
  expect_message(read_bds(jtf[13]),
    "BDS 110 (Geboortegewicht in grammen: heeft geen waarde",
    fixed = TRUE
  )
})

test_that("test14.json (empty file) ERROR", {
  expect_error(read_bds(jtf[14]), "premature EOF")
})

test_that("test15.json (Bdsnummer 62 numeric) MESS", {
  expect_message(read_bds(jtf[15]))
})

test_that("test16.json (Bdsnummer 20 numeric) PASSES", {
  expect_silent(read_bds(jtf[16]))
})

test_that("test17.json (Bdsnummer 82 numeric) PASSES", {
  expect_silent(read_bds(jtf[17]))
})

test_that("test18.json (Bdsnummer 91 numeric) MESS", {
  expect_message(
    read_bds(jtf[18]),
    '[{"bdsnummer":91,"description":"Smoking during pregnancy","expected":"one of: 1, 2, 99","supplied":"1","supplied_type":"numeric"}]'
  )
})

test_that("test19.json (Bdsnummer 110 numeric) PASSES", {
  expect_silent(read_bds(jtf[19]))
})

test_that("test20.json (missing Groepen) MESS", {
  expect_message(read_bds(jtf[20]),
    "Missing 'ClientGegevens$Groepen'",
    fixed = TRUE
  )
})

test_that("test21.json (minimal data) MESS", {
  expect_message(read_bds(jtf[21]),
    "Missing 'Contactmomenten'",
    fixed = TRUE
  )
})

test_that("test22.json (gad (=49) out of range 50-350: set to NA)", {
  expect_message(read_bds(jtf[22]))
})

test_that("test23.json (multiple messages) MESS", {
  expect_message(read_bds(jtf[23]))
})

test_that("test24.json (new DDI fields) SILENT", {
  expect_silent(read_bds(jtf[24]))
})

fn <- system.file("extdata", "smocc", "Laura_S.json", package = "jamestest")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Laura_S.json is silent with GA in days", {
  expect_silent(read_bds(js))
})

# 2 problematic json files identified by Allegro Sultum - Feb 2020
fn <- system.file("extdata", "test", "not_a_vector.json", package = "jamestest")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("not_a_vector.json produces messages", {
  expect_message(read_bds(js),
    "BDS 82",
    fixed = TRUE
  )
})

# problematic json file http400.json identified by Allegro Sultum - Feb 2020
fn <- system.file("extdata", "test", "http400.json", package = "jamestest")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("http400.json proceeds silent - no biological mother", {
  expect_silent(read_bds(js))
})

# Check proper splitting of head lag item
fn  <- system.file("extdata", "test", "test25.json", package = "jamestest")
tgt <- suppressMessages(read_bds(fn, append_ddi = TRUE))
test_that("D-score for time point 0.0903 is 14.14 (not 26) (test25.json", {
  expect_equal(tgt$y[1], 14.14)
})

# test battery - comment out to activate
# path <- system.file("extdata", package = "jamestest")
# libs <- c("allegrosultum", "test", "smocc", "terneuzen", "preterm", "graham")
# for (lib in libs) {
#   files <- list.files(path = file.path(path, lib), pattern = ".json", full.names = TRUE)
#   for (file in files) {
#     cat("File ", file, "\n")
#     if (file == "/Users/buurensv/Library/R/4.0/library/jamestest/extdata/test/test14.json") next
#     if (file == "/Users/buurensv/Library/R/4.0/library/jamestest/extdata/test/test8.json") next
#     js  <- jsonlite::toJSON(jsonlite::fromJSON(file), auto_unbox = TRUE)
#     test_that(paste(file, "passes"), {
#       expect_silent(suppressMessages(read_bds(txt = js)))
#     })
#   }
# }

# other stuff

# Kevin S: Check D-score and DAZ
#fn <- system.file("extdata", "smocc", "Kevin_S.json", package = "jamestest")
#ind <- read_bds(fn)

# tgt %>%
#   dplyr::filter(substr(.data$yname, 1, 3) == "ddi") %>%
#   dplyr::select(age, yname, y)

