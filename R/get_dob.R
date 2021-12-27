#' Get the date of birth
#'
#' @param tgt A list with elements `psn` and `xyz`
#' @param which String: Child ("00"), father ("01") or mother ("02")
#' @return  Object of class \code{Date}. If dob is missing,
#' the function return dob 01-01-2000 (for child).
#' @note Function is local to the `bdsreader` package, not exported.
get_dob <- function(tgt, which = "00") {
  stopifnot(is.list(tgt))
  psn <- persondata(tgt)
  if (which == "00") {
    dob <- as.Date(psn[["dob"]], format = "%d-%m-%y")
    if (is.na(dob)) {
      return(as.Date("01-01-00", format = "%d-%m-%y"))
    }
    return(dob)
  }

  dob <- as.Date(NA)

  if (which == "01") {
    dob <- as.Date(psn[["dobf"]], format = "%d-%m-%y")
  }

  if (which == "02") {
    dob <- as.Date(psn[["dobm"]], format = "%d-%m-%y")
  }

  dob
}
