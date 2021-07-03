#' Extract person-level information
#'
#' @param x A tibble with a `person` attribute
#' @return A tibble with person-level data
#' @examples
#' fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
#' persondata(read_bds(fn, version = 1))
#' @export
persondata <- function(x) {
  p <- attr(x, "person")
  if (is.null(p)) {
    stop("Found no person attribute.")
  }
  p %>% select(-all_of(c("dob")))
}
