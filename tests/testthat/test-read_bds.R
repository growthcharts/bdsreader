# Reading data with older format (bds_v1.0)
data1 <- system.file("extdata/bds_v1.0/smocc/Laura_S.json",
                     package = "jamesdemodata")
t <- read_bds(data1)

# Read file with input data according to format "3.0".
data2 <- system.file("extdata/bds_v3.0/smocc/Laura_S.json",
                     package = "jamesdemodata")
q <- read_bds(data2, format = "3.0")

test_that("Format 1.0 and 3.0 produce both D-score", {
  expect_equal(table(t$xyz$yname), table(q$xyz$yname))
})

fn2 <- system.file("extdata", "bds_v3.0", "smocc", "Laura_S.json",
                   package = "jamesdemodata")
js2b <- jsonlite::toJSON(jsonlite::fromJSON(fn2), auto_unbox = TRUE)
test_that("Format 3.0 is read after toJSON(fromJSON(filename))", {
  expect_type(read_bds(js2b), "list")
})
