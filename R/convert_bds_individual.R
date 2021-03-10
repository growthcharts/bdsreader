#' Convert json BDS data for single individual to class individual
#'
#' This function takes data from a json source and saves it as a an object
#' of class individual. The function automatically calculates
#' standard deviation scores and broken stick conditional means per visit.
#' The function hard-codes the transformations \code{nlreferences::transform2z()}
#' and \code{nlreferences::transform2y()}.
#' @param txt a JSON string, URL or file
#' @param schema A JSON string, URL or file that selects the JSON validation
#' schema.
#' @param \dots Additional parameter passed down to
#'   \code{fromJSON(txt, ...)}, \code{new("xyz",... )} and
#'   \code{new("bse",... )}. Useful parameters are \code{models =
#'   "bsmodel"} for setting the broken stick model, or \code{call =
#'   as.call(...)} for setting proper reference standards.
#' @return An object of class individual or \code{NULL}
#' @author Stef van Buuren 2020
#' @seealso \code{\link[jsonlite]{fromJSON}}
#' @examples
#' fn <- system.file("extdata", "allegrosultum", "client3.json", package = "jamestest")
#' # q <- convert_bds_individual(fn)
#' @export
convert_bds_individual <- function(txt = NULL, schema = NULL, ...) {

  if (is.null(txt)) return(NULL)

  # Check. Tranform json errors (e.g. no file, invalid json) into a
  # warning, and exit with empty individual object.
  checked <- tryCatch(
    expr = verify(txt, schema = schema, ...),
    error = function(cnd) {
      stop(conditionMessage(cnd))
    }
  )

  # and produce proper individual data object
  convert_checked_individual(checked, ...)
}
