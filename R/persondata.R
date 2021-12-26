#' Extract person-level information
#'
#' @param x An object of class `target`
#' @return A tibble with person-level data
#' @examples
#' fn <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
#' persondata(read_bds(fn))
#' @export
persondata <- function(x) {
  stopifnot(inherits(x, "target"))
  return(x$psn)
}

#' Extract time-level information
#'
#' @param x An object of class `target`
#' @return A tibble with time-level data
#' @examples
#' fn <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
#' timedata(read_bds(fn))
#' @export
timedata <- function(x) {
  stopifnot(inherits(x, "target"))
  return(x$xyz)
}
