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
        "verplicht BDS nummer ontbreekt:",
        ((unlist(w[w$keyword == "contains", "schema"])))
        [grep("[0-9].*", ((unlist(w[w$keyword == "contains", "schema"]))))]) # remove non-numbers
      )
  }

  # For misspecified BDS values
  if ("anyOf" %in% w$keyword) {
    mess$required <- c(
      mess$required,
      "foutieve BDS waarden gevonden"
    )

    # For misspecified values - return supplied and accepted values
    if (length(w[w$keyword == "anyOf" & !grepl("/clientMeasurements", w$dataPath), "data"])) {
      val.err <- t(simplify2array(w[w$keyword == "anyOf" & !grepl("/clientMeasurements", w$dataPath), "data"]))
    } else {
      val.err <- data.frame()
    }



    # find specific items in clientMeasurements with issues
    cmlist <- w[w$keyword == "anyOf" & grepl("/clientMeasurements", w$dataPath), c("instancePath", "data")]
    val.err.cm <- data.frame(
      dataPath = unlist(cmlist[, 1L]),
      bdsNumber = unlist(cmlist[,2L ])[names(unlist(cmlist[,2L ])) == "bdsNumber"]
    )
    if (nrow(val.err.cm) >= 1L) {
      # get specific values with issues
      val.err.cm <- w[!is.na(w$parentSchema$type)  & grepl("/clientMeasurements", w$dataPath), c("dataPath", "data")] %>%
        mutate(dataPath = gsub("(/clientMeasurements/[0-9]).*", replacement = "\\1", x = .data$dataPath)) %>%
        right_join(val.err.cm, by = "dataPath") %>%
        select("bdsNumber", value = "data")
      if(ncol(val.err) >= 1L) {val.err <- rbind(val.err, val.err.cm)
      } else {
        val.err <- val.err.cm
      }
    }



    # FIXME
    # creating the warning data frame does not work for JSON string, format 2.0
    # the hack below escape the creation of the supplied entry
    if (length(val.err[1, 1][[1]]) == 3) {
      return(mess)
    }

    # pick up normal processing
    if (ncol(val.err) >= 1L) {
      user.warning <- data.frame()
      for (i in 1L:nrow(val.err)) {
        user.warning[i, "bdsnummer"] <- val.err[i, 1L][[1L]]
        user.warning[i, "supplied"] <- ifelse(is.null(val.err[i, 2L][[1L]]),
          NA, as.character(val.err[i, 2L][[1L]])
        )
        user.warning[i, "supplied_type"] <- ifelse(is.null(val.err[i, 2L][[1L]]),
          NA, mode(val.err[i, 2L][[1L]])
        )
      }
      mess$supplied <- merge(user.warning, bdsreader::bds_lexicon, by = "bdsnummer") %>%
        select(all_of(c("bdsnummer", "description", "expected", "supplied", "supplied_type")))
    }
  }
  mess
}
