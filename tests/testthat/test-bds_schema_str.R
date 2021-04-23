schema <- "bds_schema.json"
schema <- "bds_schema_str.json"
pad <- ifelse(schema == "bds_schema_str.json", "_str", "")
jtf <- system.file("extdata", paste0("bds", pad), "test", paste0("test", 1:25, ".json"), package = "jamesdemodata")

test_that("test1.json (client3.json) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[1], schema = schema))
})

test_that("test2.json (missing Referentie) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[2], schema = schema))
})

test_that("test3.json (missing OrganisatieCode) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[3], schema = schema))
})

test_that("test4.json (wrong type) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[4], schema = schema))
})

test_that("test5.json (missing ClientGegevens) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[5], schema = schema))
})

test_that("test6.json (Missing ContactMomenten) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[6], schema = schema))
})

test_that("test7.json (Missing Referentie & OrganisatieCode) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[7], schema = schema))
})

# JSON SYNTAX ERROR: c++ exception (unknown reason)
# test_that("test8.json (Invalid OrganisatieCode number) FAILS bds_schema_str.json",
#          expect_false(validate_json(jtf[8])))

test_that("test9.json (Bdsnummer 19 missing) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[9], schema = schema))
})

test_that("test10.json (Bdsnummer 20 missing) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[10], schema = schema))
})

test_that("test11.json (Bdsnummer 82 missing) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[11], schema = schema))
})

test_that("test12.json (Bdsnummer 91 missing) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[12], schema = schema))
})

test_that("test13.json (Bdsnummer 110 missing) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[13], schema = schema))
})

test_that("test14.json (Empty file) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[14], schema = schema))
})

test_that("test15.json (Bdsnummer 19 numeric) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[15], schema = schema))
})

test_that("test16.json (Bdsnummer 20 numeric) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[16], schema = schema))
})

test_that("test17.json (Bdsnummer 82 numeric) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[17], schema = schema))
})

test_that("test18.json (Bdsnummer 91 numeric) FAILS bds_schema_str.json", {
  expect_false(validate_json(jtf[18], schema = schema))
})

test_that("test19.json (Bdsnummer 110 numeric) PASSES bds_schema_str.json", {
  expect_true(validate_json(jtf[19], schema = schema))
})

test_that("test20.json (missing Groepen) passes bds_schema_str.json", {
  expect_true(validate_json(jtf[20], schema = schema))
})

test_that("minimal test21.json passes bds_schema_str.json", {
  expect_true(validate_json(jtf[21], schema = schema))
})

fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Laura_S.json passes bds_schema_str.json", {
  expect_true(validate_json(fn, schema = schema))
})

fn <- system.file("extdata", paste0("bds", pad), "graham", "Bas_G.json", package = "jamesdemodata")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Bas_G.json (Bdsnummer 19 missing) FAILS bds_schema_str.json", {
  expect_false(validate_json(fn, schema = schema))
})

