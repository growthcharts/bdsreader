schemas <- c(system.file("schemas/bds_v1.0.json", package = "bdsreader", mustWork = TRUE),
             system.file("schemas/bds_v2.0.json", package = "bdsreader", mustWork = TRUE))
paths <-   c("bds_v1.0",
             "bds_v2.0")

for (i in 1:2) {
  schema <- schemas[i]
  path <- paths[i]

  jtf <- system.file("extdata", path, "test", paste0("test", 1:25, ".json"), package = "jamesdemodata")

  # test the empty object
  if (i == 1) {
    js1 <- '{"OrganisatieCode":0,"ClientGegevens":{}}'
    test_that("handles the empty individual object", {
      expect_message(
        verify(js1, schema = schema),
        "must have required property 'Elementen'"
      )
    })
  }

  if (i == 2) {
    js1 <- '{"OrganisatieCode":0,"ClientGegevens":[]}'
    test_that("handles the empty individual object", {
      expect_message(
        verify(js1, schema = schema),
        "verplicht BDS nummer ontbreekt: 19"
      )
    })
  }


  test_that("test1.json (client3.json) passes convert_individual_bds()", {
    expect_silent(verify(jtf[1], schema = schema))
  })

  test_that("test2.json (missing Referentie) PASSES", {
    expect_silent(verify(jtf[2], schema = schema))
  })

  if (i == 1) {
    test_that("test3.json (missing OrganisatieCode) WARNS", {
      expect_message(
        verify(jtf[3], schema = schema),
        "must have required property 'OrganisatieCode'"
      )
    })
  }

  if (i == 2) {
    test_that("test3.json OrganisatieCode optional", {
      expect_silent(verify(jtf[3], schema = schema))
    })
  }

  if (i == 1) {
    test_that("test4.json (wrong type) - silent OK", {
      expect_silent(
        verify(jtf[4], schema = schema))
    })
  }

  if (i == 1) {
    test_that("test5.json (missing ClientGegevens) WARNS", {
      expect_message(
        verify(jtf[5], schema = schema),
        "must have required property 'ClientGegevens'"
      )
    })
  }

  if (i == 2) {
    test_that("test5.json (missing ContactMomenten) WARNS", {
      expect_message(
        verify(jtf[5], schema = schema),
        "Missing 'ContactMomenten'"
      )
    })
  }

  if (i == 1) {
    test_that("test6.json (Missing ContactMomenten) WARNS", {
      expect_message(verify(jtf[6], schema = schema),
                     "Missing 'Contactmomenten'",
                     fixed = TRUE
      )
    })
  }

  if (i == 2) {
    test_that("test6.json BDS 235 (Lengte in mm): heeft geen waarde", {
      expect_message(verify(jtf[6], schema = schema),
                     "BDS 235 (Lengte in mm): heeft geen waarde",
                     fixed = TRUE
      )
    })
  }

  if (i == 1) {
    test_that("test7.json (Missing Referentie & OrganisatieCode) WARNS", {
      expect_message(
        verify(jtf[7], schema = schema),
        "must have required property 'OrganisatieCode'"
      )
    })
  }

  if (i == 2) {
    test_that("test7.json (Missing Referentie & OrganisatieCode) optional", {
      expect_silent(
        verify(jtf[7], schema = schema)
      )
    })
  }


  test_that("test8.json (Invalid JSON) ERROR", {
    expect_error(
      verify(jtf[8], schema = schema),
      "lexical error: invalid char in json text."
    )
  })

  if (i == 1) {
    test_that("test9.json (Bdsnummer 19 missing) returns warning", {
      expect_message(
        verify(jtf[9], schema = schema),
        "verplicht BDS nummer ontbreekt: 19"
      )
    })
  }

  if (i == 1) {
    test_that("test10.json (Bdsnummer 20 missing) return warning", {
      expect_message(
        verify(jtf[10], schema = schema),
        "verplicht BDS nummer ontbreekt: 20"
      )
    })
  }

  if (i == 2) {
    test_that("test10.json (Bdsnummer 20 missing) optional, Missing 'ContactMomenten'", {
      expect_message(
        verify(jtf[10], schema = schema),
        "Missing 'ContactMomenten'"
      )
    })
  }

  test_that("test11.json (Bdsnummer 82 missing) return message", {
    expect_message(verify(jtf[11], schema = schema),
                   "BDS 82 (Zwangerschapsduur in dagen) heeft geen waarde",
                   fixed = TRUE
    )
  })

  test_that("test12.json (Bdsnummer 91 missing) PASSES", {
    expect_silent(verify(jtf[12], schema = schema))
  })

  test_that("test13.json (Bdsnummer 110 missing) returns message", {
    expect_message(verify(jtf[13], schema = schema),
                   "BDS 110 (Geboortegewicht in grammen: heeft geen waarde",
                   fixed = TRUE
    )
  })

  test_that("test14.json (empty file) returns ERROR", {
    expect_error(verify(jtf[14], schema = schema), "premature EOF")
  })

  if (i == 1) {
    test_that("test15.json (Bdsnummer 19 numeric) returns message", {
      expect_message(
        verify(jtf[15], schema = schema),
        '[{"bdsnummer":19,"description":"Sex of child","expected":"one of: 0, 1, 2, 3","supplied":"2","supplied_type":"numeric"},{"bdsnummer":62,"description":"Caretaker relation","expected":"one of: 01, 02, 03, 04, 05, 06, 07, 08, 98","supplied":"1","supplied_type":"numeric"}]'
      )
    })
  }

  test_that("test16.json (Bdsnummer 20 numeric) PASSES", {
    expect_silent(verify(jtf[16], schema = schema))
  })

  test_that("test17.json (Bdsnummer 82 numeric) PASSES", {
    expect_silent(verify(jtf[17], schema = schema))
  })

  if (i == 1) {
    test_that("test18.json (Bdsnummer 91 numeric) produces message", {
      expect_message(
        verify(jtf[18], schema = schema),
        '[{"bdsnummer":91,"description":"Smoking during pregnancy","expected":"one of: 1, 2, 99","supplied":"1","supplied_type":"numeric"}]'
      )
    })
  }

  test_that("test19.json (Bdsnummer 110 numeric) PASSES", {
    expect_silent(verify(jtf[19], schema = schema))
  })

  if (i == 1) {
    test_that("test20.json (missing Groepen) produces message", {
      expect_message(verify(jtf[20], schema = schema),
                     "Missing 'ClientGegevens$Groepen'",
                     fixed = TRUE
      )
    })
  }

  if (i == 2) {
    test_that("test20.json (missing Groepen) produces message", {
      expect_silent(verify(jtf[20], schema = schema))
    })
  }

  test_that("test21.json (minimal data) WARNS", {
    expect_message(verify(jtf[21], schema = schema),
                   "Missing 'Contact",
                   fixed = TRUE
    )
  })

  test_that("test22.json (range checking) PASSES", {
    expect_message(verify(jtf[22], schema = schema))
  })

  test_that("test23.json (multiple messages) produces message", {
    expect_message(verify(jtf[23], schema = schema))
  })

  test_that("test24.json (new DDI fields) PASSES", {
    expect_silent(verify(jtf[24], schema = schema))
  })

  if (i == 1) {
    fn <- system.file("extdata", path, "smocc", "Laura_S.json", package = "jamesdemodata")
    js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

    test_that("Laura_S.json is silent with GA in days", {
      expect_silent(verify(js, schema = schema))
    })
  }

  # 2 problematic json files identified by Allegro Sultum - Feb 2020
  fn <- system.file("extdata", path, "test", "not_a_vector.json", package = "jamesdemodata")
  js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

  test_that("not_a_vector.json produces messages", {
    expect_message(verify(js))
  })

  # problematic json file http400.json identified by Allegro Sultum - Feb 2020
  fn <- system.file("extdata", path, "test", "http400.json", package = "jamesdemodata")
  js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)

  if (i == 1) {
    test_that("http400.json proceeds silent - no biological mother", {
      expect_silent(verify(js))
    })
  }

  if (i == 2) {
    test_that("http400.json proceeds silent - no biological mother", {
      expect_message(verify(js))
    })
  }


  # test battery - comment out to activate
  # path <- system.file("extdata", path, package = "jamesdemodata")
  # libs <- c("allegrosultum", "test", "smocc", "terneuzen", "preterm", "graham")
  # for (lib in libs) {
  #   files <- list.files(path = file.path(path, lib), pattern = ".json", full.names = TRUE)
  #   for (file in files) {
  #     cat("File ", file, "\n")
  #     if (file == "/Users/buurensv/Library/R/4.0/library/jamesdemodata/extdata/test/test14.json") next
  #     if (file == "/Users/buurensv/Library/R/4.0/library/jamesdemodata/extdata/test/test8.json") next
  #     js  <- jsonlite::toJSON(jsonlite::fromJSON(file), auto_unbox = TRUE)
  #     test_that(paste(file, "passes"), {
  #       expect_silent(suppressMessages(verify(txt = js)))
  #     })
  #   }
  # }

}
