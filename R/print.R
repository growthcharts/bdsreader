#' @exportS3Method
#' @method print bdsreader
print.bdsreader <- function(x, ...) {
  cat("<bdsreader>\n")
  psn <- x$psn
  keep <- setdiff(names(psn), c("dob", "dobf", "dobm", "etn", "pc4"))
  psn <- psn[, keep, drop = FALSE]
  cat("$psn:\n")
  print(as.data.frame(psn))
  cat("\n$xyz:\n")
  print(x$xyz)
  invisible(x)
}
