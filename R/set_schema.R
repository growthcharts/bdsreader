#' Set the schema from user input
#'
#' @param format Integer. JSON schema format number. There are currently two
#'   schemas supported, formats `1` and `2`.
#' @param schema A file name (including path) with the JSON validation schema.
#'   The `schema` argument overrides `format`. The function extracts the
#'   character following the string `"_v"` in the base name, and overwrites the
#'   `format` argument by the integer representation of the found character.
#' @return A list with components `format`, `schema` and `schema_base`.
#' @examples
#' set_schema(1)
#' set_schema(schema = "mydir/myschema_v1.0.json")
#' @export
set_schema <- function(format = 2L, schema = NULL) {
  if (is.null(schema)) {
    schema_base <- switch(as.character(format),
                          "1" = "bds_v1.0.json",
                          "2" = "bds_v2.0.json",
                          "bds_v2.0.json")
    schema <- system.file("schemas", schema_base, package = "bdsreader", mustWork = TRUE)
  } else {
    # extract format by file name extract
    schema_base <- basename(schema)
    format <- as.integer(substr(strsplit(schema_base, "_v")[[1]][2], 1, 1))
  }
  list(format = format, schema = schema, schema_base = schema_base)
}
