# Reading data with older format (bds_v1.0)
data1 <- system.file("extdata/bds_v1.0/smocc/Laura_S.json", package = "jamesdemodata")
t <- read_bds(data1)

# Read file with input data according to format "2.0".
data2 <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
q <- read_bds(data2, format = "2.0")

test_that("Format 1.0 and 2.0 produce both D-score", {
  expect_equal(table(t$yname), table(q$yname))
})

fn2 <- system.file("extdata", "bds_v2.0", "smocc", "Laura_S.json", package = "jamesdemodata")
js2a <- readLines(fn2)
js2b <- jsonlite::toJSON(jsonlite::fromJSON(fn2), auto_unbox = TRUE)

test_that("Format 2.0 is read after readLines(filename)", {
  expect_type(read_bds(js2a), "list")
})

test_that("Format 2.0 is read after toJSON(fromJSON(filename))", {
  expect_type(read_bds(js2b), "list")
})

# Read from URL
#test_that("Reads URL", {
#  expect_type(read_bds(txt = "http://localhost/ocpu/library/jamesdemodata/extdata/bds_v2.0/smocc/Laura_S.json"), "list")
#})

