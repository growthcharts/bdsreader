#' Is this object of class `target`?
#'
#' @param x An object
#' @return A logical
#' @export
is.target <- function(x) {
  inherits(x, "target")
}
