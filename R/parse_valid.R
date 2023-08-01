parse_valid <- function(valid) {
  mess <- list(
    required = character(0),
    supplied = character(0)
  )
  if (valid) {
    return(mess)
  }

  # extract error information
  w <- attr(valid, "error")

  # Error messages for errors outside of BDS values
  # required field error
  if (any(!grepl("contains", w[w$keyword == "required", "schemaPath"]))) {
    mess$required <- w[w$keyword == "required" &
                         !grepl("contains", w$schemaPath), "message"]

  }

  # date errors
  if (any(!grepl("anyOf", w[w$keyword == "pattern", "schemaPath"]))) {
    mess$required <- c(
      mess$required,
      paste("Tijdstip is verkeerd ingevoerd:",
            w[w$keyword == "pattern" & !grepl("anyOf", w$schemaPath), "data"],
            collapse = " ")
    )
  }

  # type errors
  if (any(!grepl("anyOf", w[w$keyword == "type", "schemaPath"]))) {
    mess$required <- c(
      mess$required,
      paste(w[
        w$keyword == "type" & !grepl("anyOf", w$schemaPath),
        c("dataPath", "message")
      ], collapse = " ")
    )
  }

  # For missing BDS numbers that are required
  if ("contains" %in% w$keyword) {
    mess$required <- c(
      mess$required,
      paste(
        "required BDS not found:",
        ((unlist(w[w$keyword == "contains", "schema"])))
        [grep("[0-9].*", ((unlist(w[w$keyword == "contains", "schema"]))))])
      )
  }

  # For misspecified BDS values
  if ("anyOf" %in% w$keyword) {
    mess$required <- c(
      mess$required,
      "misspecified BDS values"
    )

    # For misspecified values - return supplied and accepted values
    if (length(w[w$keyword == "anyOf" & !grepl("/clientMeasurements",
                                               w$dataPath), "data"])) {
      val_err <- t(simplify2array(
        w[w$keyword == "anyOf" & !grepl("/clientMeasurements", w$dataPath),
          "data"]))
    } else {
      val_err <- NULL
    }


    # find errors in clientMeasurements
    if (length(w[w$keyword == "anyOf" & grepl("/clientMeasurements",
                                              w$dataPath), "data"])) {
      val_err_cm <-
        w[w$keyword == "anyOf" & grepl("/clientMeasurements", w$dataPath),
          "data"]
      # check for bdsNumbers, values not required
      val_err_cm <- lapply(val_err_cm, function(x) {
        if (!"bdsNumber" %in% names(x)) return(NULL)
        if (!"values" %in% names(x)) x$values <- data.frame()
        return(x)
      })
      val_err_cm[sapply(val_err_cm, is.null)] <- NULL
      if (length(val_err_cm) >= 1L) {
        val_err_cm <- t(simplify2array(val_err_cm))
      } else {
        val_err_cm <- NULL
        }

      if (is.null(val_err)) {
        val_err <- val_err_cm
      } else {
        val_err <- rbind(val_err, val_err_cm)
      }
    }

    if (is.null(val_err)) {
      return(mess)
    }

    # FIXME
    # creating the warning data frame does not work for JSON string,
    # format 2.0
    # the hack below escape the creation of the supplied entry
    if (length(val_err[1, 1][[1]]) == 3) {
      return(mess)
    }

    # pick up normal processing
    if (ncol(val_err) >= 1L) {
      user_warning <- data.frame()
      for (i in seq_len(nrow(val_err))) {
        user_warning[i, "bdsnummer"] <- val_err[i, 1L][[1L]]
        user_warning[i, "supplied"] <- ifelse(is.null(val_err[i, 2L][[1L]]),
          NA, as.character(val_err[i, 2L][[1L]])
        )
        user_warning[i, "supplied_type"] <-
          ifelse(is.null(val_err[i, 2L][[1L]]), NA, mode(val_err[i, 2L][[1L]])
        )
        # for clientMeasurement errors
        if (mode(val_err[i, 2L][[1L]]) == "list") {
          user_warning[i, "supplied"] <- "one or more supplied values"
        }
      }
      mess$supplied <-
        merge(user_warning, bdsreader::bds_lexicon, by = "bdsnummer") %>%
        select(all_of(
          c("bdsnummer", "description_EN", "expected", "supplied",
            "supplied_type")))
    }
  }
  mess
}
