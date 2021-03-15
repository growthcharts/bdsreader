#' Convert json BDS data for single individual to target object
#'
#' This function takes data from a json source and saves it as a an object
#' of class `target`.
#' @param txt a JSON string, URL or file
#' @param schema A JSON string, URL or file that selects the JSON validation
#' schema.
#' @param \dots Additional parameter passed down to `fromJSON(txt, ...)`.
#' @return An object of class `target` or `NULL`
#' @author Stef van Buuren 2021
#' @seealso [jsonlite::fromJSON()]
#' @examples
#' fn <- system.file("extdata", "allegrosultum", "client3.json", package = "jamestest")
#' q <- convert_bds_target(fn)
#' @export
convert_bds_target <- function(txt = NULL, schema = NULL, ...) {
  if (is.null(txt)) {
    return(NULL)
  }

  # Check. Tranform json errors (e.g. no file, invalid json) into a
  # warning, and exit with empty target object.
  checked <- tryCatch(
    expr = verify(txt, schema = schema, ...),
    error = function(cnd) {
      stop(conditionMessage(cnd))
    }
  )

  # parse to list with child/time components
  x <- convert_checked_list(checked)

  # add Z-scores
  data <- x$time %>%
    mutate(sex = (!!x)$child$sex,
           ga = (!!x)$child$ga,
           bmi = .data$wgt / (.data$hgt / 100)^2)
  data <- bind_cols(
    select(data, c(-c("sex", "ga"))),
    transform2z(data))

  val <- list(child = x$child, time = data)
  class(val) <- c("target", "list")
  val
}
