#' Verify validity of incoming JSON data
#'
#' This function checks the json input, runs it against a validation schema, and
#' checks the input json for further processing.
#' @param txt a JSON string, URL or file
#' @param schema A JSON string, URL or file that selects the JSON validation
#' schema.
#' @param v version number for schema.
#' @param \dots Additional parameter passed down to
#'   `fromJSON(txt, ...)`.
#' @return A list of processed input data. Side effect: messages are
#' thrown for input fields that do not conform to the expected
#' data.
#' @author Stef van Buuren 2020
#' @seealso [jsonlite::fromJSON()]
#' @examples
#' fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
#' p <- verify(fn)
#' @export
verify <- function(txt = NULL, schema = NULL, v = 1, ...) {

  # PHASE 1: check JSON syntax: if needed, warn and exit
  err <- catch_cnd(data <- fromJSON(txt, ...))
  if (!is.null(err)) stop(conditionMessage(err))

  # PHASE 2: JSON schema validation
  valid <- validate_json(txt, schema)
  mess <- parse_valid(valid)

  if (length(mess$required) > 0L) {
    if (any(grepl("required", mess$required)) ||
      any(grepl("verplicht", mess$required)) ||
      any(grepl("should", mess$required))) {
      throw_messages(mess$required)
    }
  }
  throw_messages(mess$supplied)

  # PHASE 3: Range checks
  ranges <- suppressWarnings(check_ranges(data, v))

  list(input = txt, data = data, ranges = ranges)
}
