#' Extract person-level information
#'
#' @param x A tibble with a `person` attribute
#' @return A tibble with person-level data
#' @examples
#' fn <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
#' persondata(read_bds(fn))
#' @export
persondata <- function(x) {
  p <- attr(x, "person")
  if (is.null(p)) {
    stop("Found no person attribute.")
  }
  p %>% select(-all_of(c("dob")))
}
