#' Write target data as bds-formatted JSON file
#'
#' This function takes data from a tibble with a person attribute
#' and saves it into JSON bds format (which can be sent to JAMES).
#'
#' Functions `read_bds()` and `write_bds()` are inverse operations.
#'
#' If the date of birth is not known, the conversion uses the
#' artificial birth date `01 Jan 2000` to calculate measurement
#' dates from age.
#' @param x Tibble with an attribute called `person`
#' @param \dots Additional parameters. Currently ignored.
#' @inheritParams validate_json
#' @return A string with bds-formatted JSON codes, or `NULL` for invalid
#' JSON
#' @author Stef van Buuren 2021
#' @seealso [jsonlite::toJSON()]
#' @examples
#' fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
#' tgt <- read_bds(fn, append_ddi = TRUE)
#' js <- write_bds(tgt)
#' @export
write_bds <- function(x = NULL, schema = NULL, ...) {
  p <- attr(x, "person")
  if (is.null(p)) {
    stop("Found no person attribute.")
  }

  # character or numeric
  type <- ifelse(is.null(schema) || schema == "bds_schema_str.json",
                 "character", "numeric"
  )

  # required elements
  bds <- list(
    OrganisatieCode = 0L,
    ClientGegevens = as_bds_clientdata(x, type)
  )

  # optional elements
  bds$Referentie <- as_bds_reference(x)
  bds$Contactmomenten <- as_bds_contacts(x, type)

  result <- toJSON(bds, auto_unbox = TRUE)
  if (!validate(result)) {
    warning("Cannot create valid JSON")
    return(NULL)
  }

  # throw messages if data deviate from schema
  v <- verify(result, schema = schema)
  result
}

as_bds_reference <- function(tgt) {
  n <- attr(tgt, "person")$name
  if (!length(n) || is.na(n)) {
    return(NULL)
  }
  n
}

as_bds_clientdata <- function(tgt, type) {
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

as_bds_contacts <- function(tgt, type) {
  # this function produces a JSON string with data coded according
  # to the BDS schema

  # extract measurements, only take age-related
  # remove duplicates, and NA's on y
  d <- tgt %>%
    filter(.data$xname == "age") %>%
    filter(!duplicated(.data)) %>%
    drop_na(.data$y)

  # return NULL if there are no measurements
  if (!nrow(d)) {
    return(NULL)
  }

  # back-calculate measurement dates
  d$time <- age_to_time(tgt, d$x)

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
    drop_na(.data$bds) %>%
    mutate(
      chr = type == "character",
      Bdsnummer = as.integer(.data$bds),
      Waarde = ifelse(.data$chr, as.character(.data$y), .data$y)) %>%
    select(all_of(c("time", "Bdsnummer", "Waarde"))) %>%
    arrange(.data$time, .data$Bdsnummer)

  # extract raw responses from DDI
  ddi <- tgt %>%
    filter(substr(.data$yname, 1L, 3L) == "bds") %>%
    mutate(
      time = age_to_time(!!tgt, .data$age),
      Bdsnummer = as.integer(substring(.data$yname, 4L)),
      Waarde = ifelse(type == "character", as.character(.data$y), .data$y)
    ) %>%
    select(all_of(c("time", "Bdsnummer", "Waarde")))

  # merge measurements
  d <- bind_rows(d, ddi) %>%
    arrange(.data$time, .data$Bdsnummer)

  # split by time, and return
  f <- as.factor(d$time)
  d <- split(d[, c("Bdsnummer", "Waarde")], f)

  data.frame(
    Tijdstip = names(d),
    Elementen = I(d),
    stringsAsFactors = FALSE
  )
}

age_to_time <- function(tgt, age) {
  # back-calculate measurement dates
  # use 2000-01-01 as birth data if no DOB is known
  dob <- as.Date(attr(tgt, "person")$dob, format = "%d-%m-%y")
  days <- round(age * 365.25)
  format(dob + days, format = "%Y%m%d")
}
