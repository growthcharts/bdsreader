#' Set the schema from user input
#'
#' @param version Integer. JSON schema version number. There are currently two
#'   schemas supported, versions `1` and `2`.
#' @param schema A file name (including path) with the JSON validation schema.
#'   The `schema` argument overrides `version`. The function extracts the
#'   character following the string `"_v"` in the base name, and overwrites the
#'   `version` argument by the integer representation of the found character.
#' @return A list with components `version`, `schema` and `schema_base`.
#' @examples
#' set_schema(1)
#' set_schema(schema = "mydir/myschema_v1.0.json")
#' @export
set_schema <- function(version = 2L, schema = NULL) {
  if (is.null(schema)) {
    schema_base <- switch(as.character(version),
                          "1" = "bds_v1.0.json",
                          "2" = "bds_v2.0.json",
                          "bds_v2.0.json")
    schema <- system.file("schemas", schema_base, package = "bdsreader", mustWork = TRUE)
  } else {
    # extract version by file name extract
    schema_base <- basename(schema)
    version <- as.integer(substr(strsplit(schema_base, "_v")[[1]][2], 1, 1))
  }
  list(version = version, schema = schema, schema_base = schema_base)
}
