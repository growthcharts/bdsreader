#' @exportS3Method
#' @method print bdsreader
print.bdsreader <- function(x, ...) {
  cat("<bdsreader>\n")
  psn <- x$psn
  keep <- setdiff(names(psn), c("dob", "dobf", "dobm", "etn", "pc4"))
  psn <- psn[, keep, drop = FALSE]
  cat("$persondata:\n")
  print(psn)
  cat("\n$timedata:\n")
  print(x$xyz)
  invisible(x)
}
