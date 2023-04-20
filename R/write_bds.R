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
#' @param x      List containing elements `psn` (persondata) and `xyz` (timedata)
#'  with data of the target child. See `bdsreader:::make_target()` for supported
#'  fields.
#' @param auto_format Logical. Should a field `Format` be written to the result?
#' Default is `TRUE`.
#' @param file   File name. The default (`NULL`) returns the json representation
#' of the data and does not write to a file.
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
#' fn <- system.file("extdata/bds_v1.0/smocc/Laura_S.json", package = "jamesdemodata")
#' tgt <- read_bds(fn, format = "1.0", append_ddi = FALSE)
#' js1 <- write_bds(tgt, format = "1.0")
#' js2 <- write_bds(tgt, format = "2.0")
#' @export
write_bds <- function(x = NULL,
                      auto_format = TRUE,
                      format = "2.0",
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

  # for v = 1: distinguish between v1.0 and v1.1
  type <- ifelse(grepl("v1.1", schema, fixed = TRUE), "numeric", "character")
  v <- as.integer(substr(format, 1L, 1L))
  if (v == 2L | v == 3L) type <- "numeric"

  # administrative elements
  bds <- list(
    Format = format,
    OrganisatieCode = as.integer(organisation),
    Referentie = as_bds_reference(x)
  )
  if (!auto_format) bds$Format <- NULL

  # data elements
  bds$ClientGegevens <- as_bds_clientdata(x, v, type)
  bds$ContactMomenten <- as_bds_contacts(x, v, type)
  if (v == 3L) {
    bds$nestedDetails <- list(
      list(
        nestingBdsNumber = 62,
        nestingCode = "01",
        clientDetails = list(
          list(
            bdsNumber = 63,
            value = format(as.Date(get_dob(x, which = "01")), format = "%Y%m%d"))
        ),
        clientMeasurements = list()
      ),
      list(
        nestingBdsNumber = 62,
        nestingCode = "02",
        clientDetails = list(
          list(
            bdsNumber = 63,
            value = format(as.Date(get_dob(x, which = "02")), format = "%Y%m%d"))
        ),
        clientMeasurements = list()
      )
    )
  }
  if (v == 1L) {
    names(bds) <- gsub("ContactMomenten", "Contactmomenten", names(bds))
  }

  js <- toJSON(bds, auto_unbox = TRUE, ...)
  switch(v,
         js <- gsub("Waarde2", "Waarde", js),
         js <- gsub("Waarde2", "Waarde", js),
         js <- gsub("value2", "value", js))
  if (v == 1L) {
    js <- gsub("ElementNummer", "Bdsnummer", js)
  }
  if (v == 3L) {
    js <- gsub("ClientGegevens", "clientDetails", js)
    js <- gsub("ContactMomenten", "clientMeasurements", js)
  }
  if (!check) {
    return(js)
  }
  if (!validate(js)) {
    warning("Cannot create valid JSON")
    return(NULL)
  }

  # throw messages if data deviate from schema
  v <- verify(js, schema = schema)

  # prettify
  if (!is.null(indent)) js <- prettify(js, indent = indent)

  # write to file
  if (!is.null(file)) {
    writeLines(text = js, con = file)
    return(invisible(js))
  }
  js
}

as_bds_reference <- function(tgt) {
  n <- persondata(tgt)[["name"]]
  if (!length(n) || is.na(n)) {
    return("Unknown")
  }
  n
}

as_bds_clientdata <- function(tgt, v, type) {
  if (v == 3L)
    return(as_bds_clientdata_v3(tgt))
  if (v == 2L)
    return(as_bds_clientdata_v2(tgt))
  as_bds_clientdata_v1(tgt, type)
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
        Waarde = ifelse(type == "character", as.character(psn$gad), as.numeric(psn$gad))
      ),
      list(
        Bdsnummer = 91,
        Waarde = ifelse(is.na(psn$smo), "99", as.character(2 - psn$smo))
      ),
      list(
        Bdsnummer = 110,
        Waarde = ifelse(type == "character", as.character(psn$bw), as.numeric(psn$bw))
      ),
      list(
        Bdsnummer = 238,
        Waarde = ifelse(type == "character", as.character(psn$hgtm * 10), as.numeric(psn$hgtm * 10))
      ),
      list(
        Bdsnummer = 240,
        Waarde = ifelse(type == "character", as.character(psn$hgtf * 10), as.numeric(psn$hgtf * 10))
      )
    ),
    Groepen = data.frame(
      rbind(
        list(Elementen = data.frame(
          Bdsnummer = as.integer(c(63, 71, 62)),
          Waarde = c(format(as.Date(get_dob(tgt, which = "01")), format = "%Y%m%d"),
                     NA_character_,
                     "01"))
        ),
        list(Elementen = data.frame(
          Bdsnummer = as.integer(c(63, 71, 62)),
          Waarde = c(format(as.Date(get_dob(tgt, which = "02")), format = "%Y%m%d"),
                     NA_character_,
                     "02"))
        )
      )
    )
  )
  # if there are no parental birth dates, strip Groepen
  if (is.na((x$Groepen[2, ])[[1]][1, 2]) && is.na((x$Groepen[2, ])[[1]][1, 2])) x$Groepen <- NULL
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
            Waarde = format(as.Date(get_dob(tgt, which = "01")), format = "%Y%m%d")
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
            Waarde = format(as.Date(get_dob(tgt, which = "02")), format = "%Y%m%d")
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

  # if there are no parental birth dates, strip Groepen
  # dobf <- x[["GenesteElementen"]][[8]][1, 2]
  # dobm <- x[["GenesteElementen"]][[9]][1, 2]
  # if (is.na(dobf) && is.na(dobm)) x$GenesteElementen <- NULL
  x
}

as_bds_clientdata_v3 <- function(tgt) {
  psn <- persondata(tgt)
  x <- list(
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

  switch(v,
         d <- split(d[, c("ElementNummer", "Waarde", "Waarde2")], f),
         d <- split(d[, c("ElementNummer", "Waarde", "Waarde2")], f),
         {
           d <- d %>%
             rename('bdsNumber' = 'ElementNummer') %>%
             rename('value' = 'Waarde') %>%
             rename('value2' = 'Waarde2') %>%
             rename('date' = 'time')
           d <- split(d[, c("date", "value", "value2")], d$bdsNumber)
         })

  # v3
  if (v ==3) {
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


age_to_time <- function(x, age) {
  # back-calculate measurement dates
  # use 2000-01-01 as birth data if no DOB is known
  dob <- as.Date(persondata(x)[["dob"]], format = "%d-%m-%y")
  days <- round(age * 365.25)
  format(dob + days, format = "%Y%m%d")
}
