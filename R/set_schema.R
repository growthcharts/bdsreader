#' Set the schema from user input
#'
#' @param format String. JSON data schema version number. There are currently
#'   three schemas supported: `1.0`, `1.1` and `2.0`.
#' @param schema A file name (optionally including the path) with the JSON
#'   validation schema.
#'   The `schema` argument overrides `format`. The function extracts the
#'   version number for the basename, and overwrites the
#'   `format` argument by version number.
#' @return A list with components `format` (either `1.0`, `1.1` or `2.0`),
#' `schema` (long file name) and `schema_base` (base file name).
#' @examples
#' set_schema("2.0")
#' @export
set_schema <- function(format = "1.0", schema = NULL) {
  if (is.null(schema)) {
    schema_base <- switch(as.character(format),
                          "1.0" = "bds_v1.0.json",
                          "1.1" = "bds_v1.1.json",
                          "2.0" = "bds_v2.0.json",
                          "-")
    if (schema_base == "-") stop("Format ", format, " unknown.")
    schema <- system.file("schemas", schema_base, package = "bdsreader", mustWork = TRUE)
  } else {
    # extract format by file name extract
    schema_base <- basename(schema)
    format <- gsub("bds_v(.+).json", "\\1", schema_base)
    schema <- system.file("schemas", schema_base, package = "bdsreader", mustWork = TRUE)
  }
  list(format = format, schema = schema, schema_base = schema_base)
}
