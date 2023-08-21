#' Write target data as bds-formatted JSON file
#'
#' This function takes a list with person and time data
#' and saves it into JSON bds format (which can be sent to JAMES).
#'
#' Functions `read_bds()` and `write_bds()` are inverse operations.
#'
#' If the date of birth is not known, the conversion uses the
#' artificial birth date `01 Jan 2000` to calculate measurement
#' dates from age.
#' @param x      List containing elements `psn` (persondata) and `xyz`
#' (timedata) with data of the target child. See `bdsreader:::make_target()`
#' for supported fields.
#' @param auto_format Logical. Should a field `Format` be written to the
#' result? Default is `TRUE`. Note: Only used for versions 1.0 and 1.1.
#' @param file   File name. The default (`NULL`) returns the json
#' representation of the data and does not write to a file.
#' @param indent Integer. Number of spaces to indent when using
#' `jsonlite::prettify()`. When not specified, the function writes
#' minified json.
#' @param organisation Integer. Organisation number of the caller. Optional.
#' @param check Logical. Should function check json to conform to schema?
#' @param verbose Logical. Print file message?
#' @param \dots Passed down to [jsonlite::toJSON()].
#' @inheritParams set_schema
#' @return A string with bds-formatted JSON codes, or `NULL` for invalid
#' JSON
#' @author Stef van Buuren 2021
#' @seealso [jsonlite::toJSON()]
#' @examples
#' fn <- system.file("extdata/bds_v1.0/smocc/Laura_S.json",
#'   package = "jamesdemodata")
#' tgt <- read_bds(fn, format = "1.0", append_ddi = FALSE)
#' js1 <- write_bds(tgt, format = "1.0")
#' js2 <- write_bds(tgt, format = "2.0")
#' js3 <- write_bds(tgt)
#' @export
write_bds <- function(x = NULL,
                      auto_format = TRUE,
                      format = "3.0",
                      schema = NULL,
                      file = NULL,
                      organisation = 0L,
                      indent = NULL,
                      check = TRUE,
                      verbose = FALSE,
                      ...) {
  stopifnot(is.list(x))
  stopifnot(all(c("psn", "xyz") %in% names(x)))

  # signal processing file
  if (!is.null(file) && !verbose) {
    message("Processing file: ", file)
  }

  schema_list <- set_schema(format = format, schema = schema)
  schema <- schema_list$schema
  format <- schema_list$format
  if (!file.exists(schema)) {
    stop("File ", schema, " not found.")
  }

  js <- switch(format,
               "1.0" = write_bds_1(x, organisation, schema, auto_format),
               "1.1" = write_bds_1(x, organisation, schema, auto_format),
               "2.0" = write_bds_2(x, organisation),
               "3.0" = write_bds_3(x, organisation),
               NULL)

  if (!check) {
    return(js)
  }
  if (!validate(js)) {
    warning("Cannot create valid JSON")
    return(NULL)
  }

  # throw messages if data deviate from schema
  v <- read_bds(js, schema = schema)

  # prettify
  if (!is.null(indent)) js <- prettify(js, indent = indent)

  # write to file
  if (!is.null(file)) {
    writeLines(text = js, con = file)
    return(invisible(js))
  }
  js
}

write_bds_1 <- function(x, organisation, schema, auto_format) {
  v <- 1
  type <- ifelse(grepl("v1.1", schema, fixed = TRUE), "numeric", "character")

  # administrative elements
  bds <- list(
    Format = ifelse(type == "numeric", "1.1", "1.0"),
    OrganisatieCode = as.integer(organisation),
    Referentie = as_bds_reference(x)
  )

  if (!auto_format) bds$Format <- NULL

  # data elements
  bds$ClientGegevens <- as_bds_clientdata(x, v, type)
  bds$ContactMomenten <- as_bds_contacts(x, v, type)
  names(bds) <- gsub("ContactMomenten", "Contactmomenten", names(bds))

  js <- toJSON(bds, auto_unbox = TRUE)
  js <- switch(v,
               gsub("Waarde2", "Waarde", js),
               gsub("Waarde2", "Waarde", js),
               gsub("value2", "value", js))
  js <- gsub("ElementNummer", "Bdsnummer", js)
  return(js)
}

write_bds_2 <- function(x, organisation) {
  v <- 2
  type <- "numeric"

  # administrative elements
  bds <- list(
    Format = "2.0",
    OrganisatieCode = as.integer(organisation),
    Referentie = as_bds_reference(x)
  )

  # data elements
  bds$ClientGegevens <- as_bds_clientdata(x, v, type)
  bds$ContactMomenten <- as_bds_contacts(x, v, type)

  js <- toJSON(bds, auto_unbox = TRUE)
  js <- switch(v,
               gsub("Waarde2", "Waarde", js),
               gsub("Waarde2", "Waarde", js),
               gsub("value2", "value", js))
  return(js)
}

write_bds_3 <- function(x, organisation) {
  v <- 3
  type <- "numeric"

  # administrative elements
  bds <- list(
    Format = "3.0",
    organisationCode = as.integer(organisation),
    reference = as_bds_reference(x)
  )

  # data elements
  bds$ClientGegevens <- as_bds_clientdata(x, v, type)
  bds$ContactMomenten <- as_bds_contacts(x, v, type)
  nestedDetails <-  as_bds_nested(x)
  bds$nestedDetails <- NULL
  if (length(nestedDetails)) bds$nestedDetails <- nestedDetails

  # remove NA fields for v3
  # clientDetails
  bds$ClientGegevens <- lapply(bds$ClientGegevens, FUN = function(x) {
    if (is.na(x[2L]) | is.null(unlist(x[2L]))) return(NULL)
    else return(x)
  })
  bds$ClientGegevens <- bds$ClientGegevens[lengths(bds$ClientGegevens) != 0L]

  js <- toJSON(bds, auto_unbox = TRUE)
  js <- switch(v,
               gsub("Waarde2", "Waarde", js),
               gsub("Waarde2", "Waarde", js),
               gsub("value2", "value", js))
  js <- gsub("ClientGegevens", "clientDetails", js)
  js <- gsub("ContactMomenten", "clientMeasurements", js)
  return(js)
}

as_bds_reference <- function(tgt) {
  n <- persondata(tgt)[["name"]]
  if (!length(n) || is.na(n)) {
    return(NA_character_)
  }
  n
}

as_bds_clientdata <- function(tgt, v, type) {
  switch(v,
         as_bds_clientdata_v1(tgt, type),
         as_bds_clientdata_v2(tgt),
         as_bds_clientdata_v3(tgt))
}

as_bds_clientdata_v1 <- function(tgt, type) {
  psn <- persondata(tgt)
  x <- list(
    Elementen = list(
      list(
        Bdsnummer = 19,
        Waarde = switch(psn$sex, "male" = "1", "female" = "2", "0")
      ),
      list(
        Bdsnummer = 20,
        Waarde = format(as.Date(get_dob(tgt)), format = "%Y%m%d")
      ),
      list(
        Bdsnummer = 82,
        Waarde = ifelse(type == "character", as.character(psn$gad),
                        as.numeric(psn$gad))
      ),
      list(
        Bdsnummer = 91,
        Waarde = ifelse(is.na(psn$smo), "99", as.character(2 - psn$smo))
      ),
      list(
        Bdsnummer = 110,
        Waarde = ifelse(type == "character", as.character(psn$bw),
                        as.numeric(psn$bw))
      ),
      list(
        Bdsnummer = 238,
        Waarde = ifelse(type == "character", as.character(psn$hgtm * 10),
                        as.numeric(psn$hgtm * 10))
      ),
      list(
        Bdsnummer = 240,
        Waarde = ifelse(type == "character", as.character(psn$hgtf * 10),
                        as.numeric(psn$hgtf * 10))
      )
    ),
    Groepen = data.frame(
      rbind(
        list(Elementen = data.frame(
          Bdsnummer = as.integer(c(63, 71, 62)),
          Waarde = c(format(as.Date(get_dob(tgt, which = "01")),
                            format = "%Y%m%d"),
                     NA_character_,
                     "01"))
        ),
        list(Elementen = data.frame(
          Bdsnummer = as.integer(c(63, 71, 62)),
          Waarde = c(format(as.Date(get_dob(tgt, which = "02")),
                            format = "%Y%m%d"),
                     NA_character_,
                     "02"))
        )
      )
    )
  )
  # if there are no parental birth dates, strip Groepen
  if (is.na((x$Groepen[2, ])[[1]][1, 2]) &&
      is.na((x$Groepen[2, ])[[1]][1, 2])) x$Groepen <- NULL
  x
}

as_bds_clientdata_v2 <- function(tgt) {
  psn <- persondata(tgt)
  x <- list(
    list(
      ElementNummer = 19,
      Waarde = switch(psn$sex, "male" = "1", "female" = "2", "0")
    ),
    list(
      ElementNummer = 20,
      Waarde = format(as.Date(get_dob(tgt)), format = "%Y%m%d")
    ),
    list(
      ElementNummer = 82,
      Waarde = as.numeric(psn$gad)
    ),
    list(
      ElementNummer = 91,
      Waarde = ifelse(is.na(psn$smo), "99", as.character(2 - psn$smo))
    ),
    list(
      ElementNummer = 110,
      Waarde = as.numeric(psn$bw)
    ),
    list(
      ElementNummer = 238,
      Waarde = as.numeric(psn$hgtm * 10)
    ),
    list(
      ElementNummer = 240,
      Waarde = as.numeric(psn$hgtf * 10)
    ),
    list(
      GenesteElementen =
        list(
          list(
            ElementNummer = 63,
            Waarde = format(as.Date(get_dob(tgt, which = "01")),
                            format = "%Y%m%d")
          ),
          list(
            ElementNummer = 71
          ),
          list(
            ElementNummer = 62,
            Waarde = "01"
          )
        )
    ),
    list(
      GenesteElementen =
        list(
          list(
            ElementNummer = 63,
            Waarde = format(as.Date(get_dob(tgt, which = "02")),
                            format = "%Y%m%d")
          ),
          list(
            ElementNummer = 71
          ),
          list(
            ElementNummer = 62,
            Waarde = "02"
          )
        )
    )
  )

  x
}

as_bds_clientdata_v3 <- function(tgt) {
  psn <- persondata(tgt)
  x <- list(
    list(
      bdsNumber = 16,
      value = ifelse(hasName(psn, "pc4"), as.character(psn$pc4), NA_character_)
    ),
    list(
      bdsNumber = 19,
      value = switch(psn$sex, "male" = "1", "female" = "2", "0")
    ),
    list(
      bdsNumber = 20,
      value = format(as.Date(get_dob(tgt)), format = "%Y%m%d")
    ),
    list(
      bdsNumber = 82,
      value = as.numeric(psn$gad)
    ),
    list(
      bdsNumber = 91,
      value = ifelse(is.na(psn$smo), "99", as.character(2 - psn$smo))
    ),
    list(
      bdsNumber = 110,
      value = as.numeric(psn$bw)
    ),
    list(
      bdsNumber = 238,
      value = as.numeric(psn$hgtm * 10)
    ),
    list(
      bdsNumber = 240,
      value = as.numeric(psn$hgtf * 10)
    ),
    list(
      bdsNumber = 471,
      value = as.integer(psn$par)
    )
  )
  x
}

as_bds_contacts <- function(x, v, type) {
  # this function produces a JSON string with data coded according
  # to the BDS schema

  # extract measurements, only take age-related
  # remove duplicates, and NA's on y
  tgt <- timedata(x)
  d <- tgt %>%
    filter(.data$xname == "age") %>%
    filter(!duplicated(.data)) %>%
    drop_na("y")

  # return NULL if there are no measurements
  if (!nrow(d)) {
    return(NULL)
  }

  # back-calculate measurement dates
  d$time <- age_to_time(x, d$x)

  # set proper units
  d[d$yname %in% c("hgt", "hdc"), "y"] <-
    d[d$yname %in% c("hgt", "hdc"), "y"] * 10
  d[d$yname == "wgt", "y"] <-
    d[d$yname == "wgt", "y"] * 1000

  # set BDS numbers
  d$bds <- recode(d$yname,
                  hgt = "235", wgt = "245", hdc = "252",
                  .default = NA_character_
  )

  # sort according to time
  d <- d %>%
    drop_na("bds") %>%
    mutate(
      ElementNummer = as.integer(.data$bds),
      Waarde = as.integer(.data$y),
      Waarde2 = as.character(.data$y)) %>%
    select(all_of(c("time", "ElementNummer", "Waarde", "Waarde2"))) %>%
    arrange(.data$time, .data$ElementNummer)

  # set proper type
  if (type == "character") d$Waarde <- NA_integer_
  if (type == "numeric") d$Waarde2 <- NA_character_

  # extract raw responses from DDI
  ddi <- tgt %>%
    filter(substr(.data$yname, 1L, 3L) == "bds") %>%
    mutate(
      time = age_to_time(!!x, .data$age),
      ElementNummer = as.integer(substring(.data$yname, 4L)),
      Waarde = NA_integer_,
      Waarde2 = as.character(.data$y)) %>%
    select(all_of(c("time", "ElementNummer", "Waarde", "Waarde2")))

  # merge measurements
  d <- bind_rows(d, ddi) %>%
    arrange(.data$time, .data$ElementNummer)

  # split by time, and return
  f <- as.factor(d$time)

  d <- switch(v,
              split(d[, c("ElementNummer", "Waarde", "Waarde2")], f),
              split(d[, c("ElementNummer", "Waarde", "Waarde2")], f),
              {d <- d %>%
                rename("bdsNumber" = "ElementNummer") %>%
                rename("value" = "Waarde") %>%
                rename("value2" = "Waarde2") %>%
                rename("date" = "time")
              split(d[, c("date", "value", "value2")], d$bdsNumber)
              })

  # v3
  if (v == 3L) {
    return(
      data.frame(
        bdsNumber = as.numeric(names(d)),
        values = I(d),
        stringsAsFactors = FALSE
      )
    )
  }
  # for v1 and v2
  data.frame(
    Tijdstip = names(d),
    Elementen = I(d),
    stringsAsFactors = FALSE
  )
}

as_bds_nested <- function(x) {

  father <- NULL

  if (!hasName(x$psn, "dobf")) {
    l1 <- NULL
  } else {
    if (is.na(x$psn$dobf)) {
      l1 <- NULL
    } else {
      l1 <- list(bdsNumber = 63,
                 value = format(x$psn$dobf, format = "%Y%m%d"))
    }
  }

  if (!hasName(x$psn, "blbf")) {
    l2 <- NULL
  } else {
    if (is.na(x$psn$blbf)) {
      l2 <- NULL
    } else {
      l2 <- list(bdsNumber = 71,
                 value = as.integer(x$psn$blbf))
    }
  }

  if (!hasName(x$psn, "eduf")) {
    l3 <- NULL
  } else {
    if (is.na(x$psn$eduf)) {
      l3 <- NULL
    } else {
      l3 <- list(bdsNumber = 66,
                 value = formatC(x$psn$eduf, width = 2, format = "d",
                                 flag = "0"))
    }
  }

  cd <- c(list(l1), list(l2), list(l3))
  cd <- cd[!sapply(cd, is.null)]
  if (length(cd)) {
    father <- list(
      nestingBdsNumber = 62,
      nestingCode = "01",
      clientDetails = cd
    )
  }

  mother <- NULL

  if (!hasName(x$psn, "dobm")) {
    l1 <- NULL
  } else {
    if (is.na(x$psn$dobm)) {
      l1 <- NULL
    } else {
      l1 <- list(bdsNumber = 63,
                 value = format(x$psn$dobm, format = "%Y%m%d"))
    }
  }

  if (!hasName(x$psn, "blbm")) {
    l2 <- NULL
  } else {
    if (is.na(x$psn$blbm)) {
      l2 <- NULL
    } else {
      l2 <- list(bdsNumber = 71,
                 value = as.integer(x$psn$blbm))
    }
  }

  if (!hasName(x$psn, "edum")) {
    l3 <- NULL
  } else {
    if (is.na(x$psn$edum)) {
      l3 <- NULL
    } else {
      l3 <- list(bdsNumber = 66,
                 value = formatC(x$psn$edum, width = 2, format = "d",
                                 flag = "0"))
    }
  }

  cd <- c(list(l1), list(l2), list(l3))
  cd <- cd[!sapply(cd, is.null)]
  if (length(cd)) {
    mother <- list(
      nestingBdsNumber = 62,
      nestingCode = "02",
      clientDetails = cd
    )
  }

  output <- list(father, mother)
  output[lengths(output) != 0L]
}

age_to_time <- function(x, age) {
  # back-calculate measurement dates
  # use 2000-01-01 as birth data if no DOB is known
  dob <- as.Date(persondata(x)[["dob"]], format = "%d-%m-%y")
  days <- round(age * 365.25)
  format(dob + days, format = "%Y%m%d")
}
