schema <- system.file("schemas/bds_v1.0.json", package = "bdsreader", mustWork = TRUE)
jtf <- system.file("extdata/bds_v1.0/test", paste0("test", 1:25, ".json"), package = "jamesdemodata")

test_that("test1.json (client3.json) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[1], schema = schema)$pass)
})

test_that("test2.json (missing Referentie) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[2], schema = schema)$pass)
})

test_that("test3.json (missing OrganisatieCode) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[3], schema = schema)$pass)
})

test_that("test4.json (wrong type) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[4], schema = schema)$pass)
})

test_that("test5.json (missing ClientGegevens) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[5], schema = schema)$pass)
})

test_that("test6.json (Missing ContactMomenten) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[6], schema = schema)$pass)
})

test_that("test7.json (Missing Referentie & OrganisatieCode) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[7], schema = schema)$pass)
})

# JSON SYNTAX ERROR: c++ exception (unknown reason)
# test_that("test8.json (Invalid OrganisatieCode number) FAILS bds_v1.0.json",
#          expect_false(verify(jtf[8])))

test_that("test9.json (Bdsnummer 19 missing) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[9], schema = schema)$pass)
})

test_that("test10.json (Bdsnummer 20 missing) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[10], schema = schema)$pass)
})

test_that("test11.json (Bdsnummer 82 missing) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[11], schema = schema)$pass)
})

test_that("test12.json (Bdsnummer 91 missing) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[12], schema = schema)$pass)
})

test_that("test13.json (Bdsnummer 110 missing) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[13], schema = schema)$pass)
})

test_that("test14.json (Empty file) FAILS bds_v1.0.json", {
  expect_error(verify(jtf[14], schema = schema)$pass)
})

test_that("test15.json (Bdsnummer 19 numeric) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[15], schema = schema)$pass)
})

test_that("test16.json (Bdsnummer 20 numeric) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[16], schema = schema)$pass)
})

test_that("test17.json (Bdsnummer 82 numeric) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[17], schema = schema)$pass)
})

test_that("test18.json (Bdsnummer 91 numeric) FAILS bds_v1.0.json", {
  expect_false(verify(jtf[18], schema = schema)$pass)
})

test_that("test19.json (Bdsnummer 110 numeric) PASSES bds_v1.0.json", {
  expect_true(verify(jtf[19], schema = schema)$pass)
})

test_that("test20.json (missing Groepen) passes bds_v1.0.json", {
  expect_true(verify(jtf[20], schema = schema)$pass)
})

test_that("minimal test21.json passes bds_v1.0.json", {
  expect_true(verify(jtf[21], schema = schema)$pass)
})

fn <- system.file("extdata/bds_v1.0/smocc/Laura_S.json", package = "jamesdemodata")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Laura_S.json passes bds_v1.0.json", {
  expect_true(verify(fn, schema = schema)$pass)
})

fn <- system.file("extdata", "bds_v1.0", "graham", "Bas_G.json", package = "jamesdemodata")
js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Bas_G.json (Bdsnummer 19 missing) FAILS bds_v1.0.json", {
  expect_false(verify(fn, schema = schema)$pass)
})

