#' Validate JSON file
#'
#' This  function takes data from a JSON source and compares it to a JSON schema
#' object. A `boolean` is returned indicating whether or not the provided
#' JSON data matches the schema.
#' @param txt A JSON string, URL or file to be compared to the schema.
#' @param schema The name of one the the built-in schema's.
#' The default (`NULL`) loads `"bds_schema_str.json"`.
#' @param verbose include an error message when validation fails.
#' @return A `boolean` with optional `errors` attribute.
#' @author Arjan Huizing 2019
#' @export
validate_json <- function(txt = NULL, schema = NULL,
                                    verbose = TRUE) {
  if (is.null(schema))
    schemajs <- system.file("json", "bds_schema_str.json", package = "bdsreader")
  else
    schemajs <- system.file("json", schema, package = "bdsreader")

  json_validate(txt, schemajs, engine = "ajv", verbose = verbose)
}
