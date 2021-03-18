#' Get the date of birth of a child
#'
#' @param x A tibble with a `child` attribute.
#' @return  Object of class `Date`. If dob is missing,
#' the function return dob 01-01-2000.
#' @export
get_dob <- function(x) {
  dob <- x$child$dob
  ifelse(is.na(dob), as.Date("01-01-00", format = "%d-%m-%y"), dob)
}
