#' Verify validity of incoming JSON data
#'
#' This function checks the json input, runs it against a validation schema, and
#' checks the input json for further processing.
#' @param schema A JSON string, URL or file with the JSON validation
#'   schema.
#' @inheritParams read_bds
#' @return A list of processed input data. Side effect: messages are
#' thrown for input fields that do not conform to the expected
#' data.
#' @author Stef van Buuren, Arjan Huizing 2021
#' @seealso [jsonlite::fromJSON()]
#' @examples
#' txt <- system.file("extdata/bds_v1.0/smocc/Laura_S.json", package = "jamesdemodata")
#' schema <- system.file("schemas/bds_v1.0.json", package = "bdsreader")
#' p <- verify(txt, schema = schema)
#' @export
verify <- function(txt, auto_format = TRUE, format = "1.0", schema = NULL) {

  # PHASE 1: check JSON syntax: if needed, warn and exit
  err <- rlang::catch_cnd(data <- fromJSON(txt))
  if (!is.null(err)) stop(conditionMessage(err))

  # PHASE 2: determine format and schema
  dfmt <- data$Format[1]
  format <- ifelse(auto_format && !is.null(dfmt), dfmt, format)
  schema_list <- set_schema(format, schema)
  format <- schema_list$format
  schema <- schema_list$schema
  if (!file.exists(schema)) {
    stop("Schema file ", schema, " not found.")
  }

  # PHASE 3: JSON schema validation
  if (is.url(txt[1L])) {
    con <- curl(txt[1L], open = "r")
    txt <- readLines(con)
    close(con)
  }
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

  # PHASE 4: Range checks
  ranges <- suppressWarnings(check_ranges(data, format))

  list(input = txt, data = data, ranges = ranges, pass = isTRUE(valid),
       schema_list = schema_list)
}

