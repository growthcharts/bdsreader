# v1.0 keep for backward compatibility
# v2.0 new schema - June 2021 - compatibility with Eaglescience
# v3.0 April 2023 - English version, compatible with Eaglescience SRM
schemas <- c(system.file("schemas/bds_v1.0.json", package = "bdsreader",
                         mustWork = TRUE),
             system.file("schemas/bds_v2.0.json", package = "bdsreader",
                         mustWork = TRUE),
             system.file("schemas/bds_v3.0.json", package = "bdsreader",
                         mustWork = TRUE))
paths <-   c("bds_v1.0",
             "bds_v2.0",
             "bds_v3.0")
validate <- formals(read_bds)$validate

for (format in c("1.0", "2.0", "3.0")) {
  v <- as.integer(substr(format, 1L, 1L))
  schema <- schemas[v]
  path <- paths[v]

  jtf <- system.file("extdata", path, "test",
                     paste0("test", 1:25, ".json"),
                     package = "jamesdemodata")

  # test the empty object
  js1 <- '{"OrganisatieCode":0,"ClientGegevens":{}}'

  if (v <= 2) {
    test_that("handles the empty individual object", {
      expect_message(
        read_bds(js1, schema = schema))
    }
    )
  }

  if (v >= 3) {
    test_that("is silent with empty object", {
      expect_silent(
        read_bds(js1, schema = schema))
    }
    )
    test_that("but produces messages with validate = TRUE", {
      expect_message(
        read_bds(js1, schema = schema, validate = TRUE))
    }
    )
  }

  z <- read_bds(jtf[1], schema = schema)

  test_that("test1.json (client3.json) passes read_bds()", {
    expect_equal(
      class(read_bds(jtf[1], schema = schema)), "list")
  })

  test_that("test2.json (missing Referentie) PASSES", {
    expect_silent(read_bds(jtf[2], schema = schema))
  })

  if (v == 1 && validate) {
    test_that("test3.json (missing OrganisatieCode) MESS", {
      expect_message(
        read_bds(jtf[3], schema = schema),
        "must have required property 'OrganisatieCode'"
      )
    })
  }

  if (v >= 2) {  # other messages are OK
    test_that("test3.json (missing OrganisatieCode) MESS", {
      expect_silent(read_bds(jtf[3], schema = schema))
    })
  }

  test_that("test4.json (wrong type) MESS", {
    expect_silent(read_bds(jtf[4], schema = schema))
  })

  if (v < 3) {
    test_that("test5.json (missing ClientGegevens) MESS", {
      expect_message(read_bds(jtf[5], schema = schema))
    })
  }

  if (v >= 3) {
    test_that("test5.json (missing ClientGegevens) silent", {
      expect_silent(read_bds(jtf[5], schema = schema))
    })
  }

  if (v < 3) {
    test_that("test6.json (Missing ContactMomenten) MESS", {
      expect_message(read_bds(jtf[6], schema = schema)
      )
    })
  }

  if (v >= 3) {
    test_that("test6.json (Missing ContactMomenten) silent", {
      expect_silent(read_bds(jtf[6], schema = schema)
      )
    })
  }

  if (v == 1  && validate) {
    test_that("test7.json (Missing Referentie & OrganisatieCode) MESS", {
      expect_message(
        read_bds(jtf[7], schema = schema),
        "must have required property 'OrganisatieCode'"
      )
    })
  }


  if (v >= 2) {  # v2.0: silent is OK
    test_that("test7.json (Missing Referentie & OrganisatieCode) MESS", {
      expect_silent(
        read_bds(jtf[7], schema = schema))
    })
  }

  test_that("test8.json (Invalid JSON) ERROR", {
    expect_message(
      read_bds(jtf[8], schema = schema),
      "lexical error: invalid char in json text.")
  })

  if (v == 1 && validate) {
    test_that("test9.json (BDS 19 missing) MESS", {
      expect_message(
        read_bds(jtf[9], schema = schema),
        "required BDS not found: 19"
      )
    })
  }

  if (v >= 2) {  #  v2.0: Silent is OK
    test_that("test9.json (BDS 19 missing) MESS", {
      expect_silent(read_bds(jtf[9], schema = schema))
    })
  }


  if (v == 1 && validate) {
    test_that("test10.json (BDS 20 missing) MESS", {
      expect_message(
        read_bds(jtf[10], schema = schema),
        "required BDS not found: 20"
      )
    })
  }

  if (v == 2) {  # v2.0: Other message OK
    test_that("test10.json ( Missing 'ContactMomenten') MESS", {
      expect_message(read_bds(jtf[10], schema = schema))
    })
  }

  if (v > 2) {  # v3.0: silent
    test_that("test10.json ( Missing 'ContactMomenten') silent", {
      expect_silent(read_bds(jtf[10], schema = schema))
    })
  }

  if (v <= 2) {
    test_that("test11.json (Bdsnummer 82 missing) MESS", {
      expect_message(read_bds(jtf[11], schema = schema)
      )
    })
  }

  if (v > 2) {
    test_that("test11.json (Bdsnummer 82 missing) silent", {
      expect_silent(read_bds(jtf[11], schema = schema)
      )
    })
  }

  test_that("test12.json (Bdsnummer 91 missing) PASSES", {
    expect_silent(read_bds(jtf[12], schema = schema))
  })

  if (v <= 2) {
    test_that("test13.json (Bdsnummer 110 missing) MESS", {
      expect_message(read_bds(jtf[13], schema = schema),
                     "BDS 110 (Birth weight in grammes): has no value",
                     fixed = TRUE)
    })
  }

  if (v >= 3) {
    test_that("test13.json (Bdsnummer 110 missing) MESS", {
      expect_silent(read_bds(jtf[13], schema = schema))
    })
  }

  test_that("test14.json (empty file) ERROR", {
    expect_message(
      read_bds(jtf[14], schema = schema),
      "lexical error: invalid char in json text.")
  })

  if (v == 1 && validate) {
    test_that("test15.json (Bdsnummer 62 numeric) message", {
      expect_message(read_bds(jtf[15], schema = schema))
    })
  }

  if (v %in% c(2, 3)) {
    test_that("test15.json (Bdsnummer 62 numeric) silent OK", {
      expect_silent(read_bds(jtf[15], schema = schema))
    })
  }

  test_that("test16.json (Bdsnummer 20 numeric) PASSES", {
    expect_silent(read_bds(jtf[16], schema = schema))
  })

  test_that("test17.json (Bdsnummer 82 numeric) PASSES", {
    expect_silent(read_bds(jtf[17], schema = schema))
  })

  if (v == 1 && validate) {
    test_that("test18.json (Bdsnummer 91 numeric) MESS", {
      expect_message(
        read_bds(jtf[18], schema = schema)
      )})
  }

  if (v %in% c(2, 3)) {
    test_that("test18.json (Bdsnummer 91 numeric) MESS", {
      expect_silent(
        read_bds(jtf[18], schema = schema)
      )})
  }

  test_that("test19.json (Bdsnummer 110 numeric) PASSES", {
    expect_silent(read_bds(jtf[19], schema = schema))
  })

  if (v == 1) {
    test_that("test20.json (missing Groepen) MESS", {
      expect_message(read_bds(jtf[20], schema = schema)
      )
    })
  }

  if (v == 2) {
    test_that("test20.json (missing Groepen) MESS", {
      expect_silent(read_bds(jtf[20], schema = schema)
      )
    })
  }

  if (v == 3) {
    test_that("test20.json (missing Groepen) silent", {
      expect_silent(read_bds(jtf[20], schema = schema))
    })
  }

  if (v == 1) {
    test_that("test21.json (minimal data) MESS", {
      expect_message(read_bds(jtf[21], schema = schema),
                     "Missing 'Contactmomenten'",
                     fixed = TRUE
      )
    })
  }

  if (v == 2) {  # v2.0: Other messages
    test_that("test21.json (minimal data) MESS", {
      expect_message(read_bds(jtf[21], schema = schema))
    })
  }

  if (v >= 3) {  # v3.0
    test_that("test21.json (minimal data) silent", {
      expect_silent(read_bds(jtf[21], schema = schema))
    })
  }

  if (v <= 2) {
    test_that("test22.json (gad (=49) out of range 50-350: set to NA)", {
      expect_message(read_bds(jtf[22], schema = schema))
    })
  }

  if (v > 2) {
    test_that("test22.json (gad (=49) out of range 50-350: set to NA)", {
      expect_silent(read_bds(jtf[22], schema = schema))
    })
  }

  if (v <= 2) {
    test_that("test23.json (multiple messages) MESS", {
      expect_message(read_bds(jtf[23], schema = schema))
    })
  }

  if (v > 2) {
    test_that("test23.json (multiple messages) MESS", {
      expect_silent(read_bds(jtf[23], schema = schema))
    })
  }

  test_that("test24.json (new DDI fields) SILENT", {
    expect_silent(read_bds(jtf[24], schema = schema))
  })

  if (v <= 2) {
    test_that("test25.json Extreme D-score first few months (Smit 20210311)", {
      expect_message(read_bds(jtf[25], schema = schema))
    })
  }

  if (v > 2) {
    test_that("test25.json Extreme D-score first few months (Smit 20210311)", {
      expect_silent(read_bds(jtf[25], schema = schema))
    })
  }

  if (v == 1) {
    fn <- system.file("extdata", path, "smocc", "Laura_S.json",
                      package = "jamesdemodata")
    js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

    test_that("Laura_S.json is silent with GA in days", {
      expect_silent(read_bds(js, schema = schema))
    })
  }

  if (v == 2 && validate) {
    fn <- system.file("extdata", path, "smocc", "Laura_S.json",
                      package = "jamesdemodata")
    js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

    test_that("Laura_S.json is silent with GA in days", {
      expect_message(read_bds(js, schema = schema))
    })
  }

  if (v == 3 && validate) {
    fn <- system.file("extdata", path, "smocc", "Laura_S.json",
                      package = "jamesdemodata")
    js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

    test_that("Laura_S.json - Throws types messages with validate", {
      expect_message(read_bds(js, schema = schema, validate = TRUE))
    })
  }

  # # 2 problematic json files identified by Allegro Sultum - Feb 2020
  if (v < 3) {
    fn <- system.file("extdata", path, "test", "not_a_vector.json",
                      package = "jamesdemodata")
    js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

    test_that("not_a_vector.json produces messages", {
      expect_message(read_bds(js, schema = schema),
                     "BDS 82",
                     fixed = TRUE
      )
    })
  }

  if (v >= 3) {
    fn <- system.file("extdata", path, "test", "not_a_vector.json",
                      package = "jamesdemodata")
    js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

    test_that("not_a_vector.json is silent", {
      expect_silent(read_bds(js, schema = schema))
    })
  }

  # problematic json file http400.json identified by Allegro Sultum - Feb 2020
  fn <- system.file("extdata", path, "test", "http400.json",
                    package = "jamesdemodata")
  js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

  if (v == 1) {
    test_that("http400.json proceeds silent - no biological mother", {
      expect_silent(read_bds(js, schema = schema))
    })
  }

  if (v == 2) {
    test_that("http400.json produces messages - no biological mother", {
      expect_message(read_bds(js, schema = schema))
    })
  }

  if (v > 2) {
    test_that("http400.json silent ", {
      expect_silent(read_bds(js, schema = schema))
    })
  }

  # Check proper splitting of head lag item
  fn  <- system.file("extdata", path, "test", "test25.json",
                     package = "jamesdemodata")

  if (v < 3) {
    test_that("Works with DDI item only, and append DDI(test25.json)", {
      expect_message(read_bds(fn, schema = schema, append_ddi = TRUE))
    })
    tgt <- read_bds(fn, schema = schema, append_ddi = TRUE)
    test_that("D-score for time point 0.0903 is 16.37 (not 26) (test25.json", {
      expect_equal(tgt$xyz$y[1], 16.37)
    })
  }

  if (v >= 3) {
    test_that("Works with DDI item only, and append DDI(test25.json)", {
      expect_silent(read_bds(fn, schema = schema, append_ddi = TRUE))
    })
    tgt <- read_bds(fn, schema = schema, append_ddi = TRUE)
    test_that("D-score for time point 0.0903 is 16.37 (not 26) (test25.json", {
      expect_equal(tgt$xyz$y[1], 16.37)
    })
  }
}

# test battery - comment out to activate
# path <- system.file("extdata", package = "jamesdemodata")
# libs <- c("allegrosultum", "test", "smocc", "terneuzen", "preterm", "graham")
# for (lib in libs) {
#   files <- list.files(path = file.path(path, lib), pattern = ".json",
#                       full.names = TRUE)
#   for (file in files) {
#     cat("File ", file, "\n")
#     js  <- jsonlite::toJSON(jsonlite::fromJSON(file), auto_unbox = TRUE)
#     test_that(paste(file, "passes"), {
#       expect_silent(suppressMessages(read_bds(txt = js)))
#     })
#   }
# }
