# Reading data with older format (bds_v1.0)
data1 <- system.file("extdata/bds_v1.0/smocc/Laura_S.json",
                     package = "jamesdemodata")
t <- read_bds(data1)

# Read file with input data according to format "3.0".
data2 <- system.file("extdata/bds_v3.0/smocc/Laura_S.json",
                     package = "jamesdemodata")
q <- read_bds(data2, format = "3.0")

test_that("Format 1.0 and 3.0 read the same number of rows", {
  expect_equal(nrow(t$xyz), nrow(q$xyz))
})

fn2 <- system.file("extdata", "bds_v3.0", "smocc", "Laura_S.json",
                   package = "jamesdemodata")
js2b <- jsonlite::toJSON(jsonlite::fromJSON(fn2), auto_unbox = TRUE)
test_that("Format 3.0 is read after toJSON(fromJSON(filename))", {
  expect_type(read_bds(js2b), "list")
})


# check that duplicate mother/father from same sex couples are filtered
library(bdsreader)

# Creating the R object equivalent of the JSON structure
json_data <- list(
  Format = list("3.0"),
  organisationCode = list(0),
  reference = list("T 5270"),
  clientDetails = list(
    list(bdsNumber = 19, value = "1"),
    list(bdsNumber = 20, value = "20000101")
  ),
  nestedDetails = list(
    list(
      nestingBdsNumber = 62,
      nestingCode = "02",
      clientDetails = list(
        list(bdsNumber = 63, value = "19720701")
      )
    ),
    list(
      nestingBdsNumber = 62,
      nestingCode = "02",
      clientDetails = list(
        list(bdsNumber = 63, value = "19730701")
      )
    ),
    list(
      nestingBdsNumber = 62,
      nestingCode = "01",
      clientDetails = list(
        list(bdsNumber = 63, value = "19710701")
      )
    )
  )
)
json_string <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
data <- bdsreader::read_bds(txt = json_string)

test_that("BDS 63 filters out duplicate mothers/fathers (#10)", {
  expect_equal(as.character(data$psn$dobm), "1972-07-01")
})

