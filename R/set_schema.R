#' Set the schema from user input
#'
#' @param format String. JSON data schema version number. There are currently
#'   three schemas supported: `1.0`, `1.1` and `2.0`.
#' @param schema A file name (including path) with the JSON validation schema.
#'   The `schema` argument overrides `format`. The function extracts the
#'   version number for the basename, and overwrites the
#'   `format` argument by version number.
#' @return A list with components `format`, `schema` and `schema_base`.
#' @examples
#' set_schema("2.0")
#'
#' # for development
#' set_schema(schema = "mydir/bds_v3.0.json")
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
  }
  list(format = format, schema = schema, schema_base = schema_base)
}
