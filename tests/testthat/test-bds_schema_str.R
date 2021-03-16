jtf <- system.file("extdata", "test", paste0("test", 1:22, ".json"), package = "jamestest")

test_that("test1.json (client3.json) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[1]))})

test_that("test2.json (missing Referentie) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[2]))})

test_that("test3.json (missing OrganisatieCode) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[3]))})

test_that("test4.json (wrong type) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[4]))})

test_that("test5.json (missing ClientGegevens) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[5]))})

test_that("test6.json (Missing ContactMomenten) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[6]))})

test_that("test7.json (Missing Referentie & OrganisatieCode) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[7]))})

# JSON SYNTAX ERROR: c++ exception (unknown reason)
#test_that("test8.json (Invalid OrganisatieCode number) FAILS bds_schema_str.json",
#          expect_false(validate_json(jtf[8])))

test_that("test9.json (Bdsnummer 19 missing) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[9]))})

test_that("test10.json (Bdsnummer 20 missing) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[10]))})

test_that("test11.json (Bdsnummer 82 missing) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[11]))})

test_that("test12.json (Bdsnummer 91 missing) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[12]))})

test_that("test13.json (Bdsnummer 110 missing) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[13]))})

test_that("test14.json (Empty file) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[14]))})

test_that("test15.json (Bdsnummer 19 numeric) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[15]))})

test_that("test16.json (Bdsnummer 20 numeric) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[16]))})

test_that("test17.json (Bdsnummer 82 numeric) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[17]))})

test_that("test18.json (Bdsnummer 91 numeric) FAILS bds_schema_str.json", {
          expect_false(validate_json(jtf[18]))})

test_that("test19.json (Bdsnummer 110 numeric) PASSES bds_schema_str.json", {
          expect_true(validate_json(jtf[19]))})

test_that("test20.json (missing Groepen) passes bds_schema_str.json", {
          expect_true(validate_json(jtf[20]))})

test_that("minimal test21.json passes bds_schema_str.json", {
          expect_true(validate_json(jtf[21]))})

fn  <- system.file("extdata", "smocc", "Laura_S.json", package = "jamestest")
js  <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

test_that("Laura_S.json passes bds_schema_str.json", {
          expect_true(validate_json(fn))})
