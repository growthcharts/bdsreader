# Reading data with older format (bds_v1.0)
data1 <- system.file("extdata/bds_v1.0/smocc/Laura_S.json", package = "jamesdemodata")
t <- read_bds(data1)

# Read file with input data according to format "2.0".
data2 <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
q <- read_bds(data2, format = "2.0")

test_that("Format 1.0 and 2.0 produce both D-score", {
  expect_equal(table(t$yname), table(q$yname))
})
