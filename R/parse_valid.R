parse_valid <- function(valid) {

  mess <- list(required = character(0),
               supplied = character(0))
  if (valid) return(mess)

  # extract error information
  w <- attr(valid, "error")

  # Error messages for errors outside of BDS values
  mess$required <- w[w$keyword == "required", "message"]

  # type errors
  if(any(!grepl("Elementen", w[w$keyword == "type", "dataPath"])))
    mess$required <- c(w[w$keyword == "required", "message"],
                       paste(w[w$keyword == "type" & !grepl("Elementen", w$dataPath),
                               c("dataPath", "message")], collapse = " "))

  # For missing BDS numbers that are required
  if("contains" %in% w$keyword)
    mess$required <- c(mess$required,
                       paste("verplicht BDS nummer ontbreekt:",
                             unlist(w[w$keyword == "contains", "schema"])))

  # For misspecified BDS values
  if("anyOf" %in% w$keyword) {
    mess$required <- c(mess$required,
                       "foutieve BDS waarden gevonden")

    # For misspecified values - return supplied and accepted values
    val.err <- t(simplify2array(w[w$keyword == "anyOf", "data"]))
    if (ncol(val.err) >= 1L) {
      user.warning <- data.frame()
      for (i in 1L:nrow(val.err)) {
        user.warning[i, "bdsnummer"] <- val.err[i, 1L][[1L]]
        user.warning[i, "supplied"] <- ifelse(is.null(val.err[i, 2L][[1L]]),
                                              NA, as.character(val.err[i, 2L][[1L]]))
        user.warning[i, "supplied_type"] <- ifelse(is.null(val.err[i, 2L][[1L]]),
                                                   NA, mode(val.err[i, 2L][[1L]]))
      }
      mess$supplied <- merge(user.warning, bdsreader::bds_lexicon, by = "bdsnummer") %>%
        select(all_of(c("bdsnummer", "description", "expected", "supplied", "supplied_type")))
    }
  }
  mess
}
