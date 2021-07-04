#' Verify validity of incoming JSON data
#'
#' This function checks the json input, runs it against a validation schema, and
#' checks the input json for further processing.
#' @param schema A JSON string, URL or file with the JSON validation
#'   schema.
#' @param \dots Additional parameter passed down to `fromJSON(txt, ...)`.
#' @inheritParams read_bds
#' @return A list of processed input data. Side effect: messages are
#' thrown for input fields that do not conform to the expected
#' data.
#' @author Stef van Buuren, Arjan Huizing 2021
#' @seealso [jsonlite::fromJSON()]
#' @examples
#' txt <- system.file("examples", "Laura_S.json", package = "bdsreader")
#' schema <- system.file("schemas/bds_v1.0.json", package = "bdsreader")
#' p <- verify(txt, schema)
#' @export
verify <- function(txt, schema, ...) {

  # PHASE 1: check JSON syntax: if needed, warn and exit
  err <- rlang::catch_cnd(data <- fromJSON(txt, ...))
  if (!is.null(err)) stop(conditionMessage(err))

  # PHASE 2: JSON schema validation
  valid <- jsonvalidate::json_validate(txt, schema, engine = "ajv", verbose = TRUE)
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
  version <- as.numeric(substr(strsplit(schema, "_v")[[1]][2], 1, 1))
  ranges <- suppressWarnings(check_ranges(data, version))

  list(input = txt, data = data, ranges = ranges, pass = isTRUE(valid))
}

