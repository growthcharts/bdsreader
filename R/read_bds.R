#' Reads selected BDS data of a person
#'
#' This function takes data from a json source, optionally validates the
#' contents against a JSON validation schema, perform checks, calculates
#' the D-score, calculates Z-scores and stores the data in an list with
#' elements `psn` and `xyz`.
#' @param txt A JSON string, URL or file
#' @param auto_format Logical. Should the format be read from the data? Default
#' is `TRUE`.
#' @param validate Logical. Should the JSON-input be validated against the
#' JSON-schema? The default (`FALSE`) bypasses checking. Set `validate = TRUE`
#' to obtain diagnostic information from the `jsonvalidate::json_validate()`
#' function.
#' @param append_ddi Should the DDI responses be appended? (only used for
#' JSON schema V1.0 and V2.0)
#' @param intermediate Logical. If `TRUE` the function writes JSON files
#' with intermediate result to the working directory.
#' 1. `input.json`: the JSON input data;
#' 2. `bds.json`: a data frame with info per BDS;
#' 3. `ddi.json`: result of recoding BDS into GSED item names;
#' 4. `psn.json`: known fixed child covariates;
#' 5. `xy.json`: time-varying variables.
#' @param verbose Show verbose output for [centile::y2z()]
#' @param \dots Ignored
#' @inheritParams set_schema
#' @return A list with elements named `"psn"` and `"xyz"`.
#' @details
#' If `txt` is unspecified or `NULL`, then the return component will `"xyz"`
#' have zero rows.
#'
#' The `format` and `schema` arguments specify the format of the JSON input
#' data argument `txt`. The default `format = "1.0"` expects that the JSON
#' input data conform to the schema specified in
#' `system.file("schemas/bds_v1.0.json", package = "bdsreader")`. This is only
#' supported for legacy. We recommend format `"3.0"`, which expects data
#' coded according to
#' `system.file("schemas/bds_v3.0.json", package = "bdsreader")`.
#'
#' The format can be specified in the JSON data file with an entry
#' named `Format`. For `auto_format == TRUE`, the data specification overrides
#' any `format` and `schema` arguments to the `read_bds()` function.
#' Schema `bds_v3.0.json` requires the `Format` field, so the correct format
#' is automatically set by the data.
#'
#' Legacy note: If you erroneously read a JSON file of format `"1.0"` using
#' format `"2.0"` you may see an error:
#' `Error ...: incorrect number of dimensions`.
#' In that make sure that you are reading with the `format = "1.0"` argument.
#' Reversely, if you erroneously read a JSON file of format `"2.0"` using format
#' `"1.0"` you may see messages `.ClientGegevens should be object` and
#' `Missing 'ClientGegevens$Groepen'`. In that case, specify `format = "2.0"`.
#'
#' @seealso [jsonlite::fromJSON()], [centile::y2z()]
#' @examples
#' fn <- system.file("examples/maria1.json", package = "bdsreader")
#' m <- read_bds(fn)
#'
#' # Assume that jamesdemodata is installed locally.
#' # If not use remotes::install_github("growthcharts/jamesdemodata")
#'
#' # Read file with input data according to format "3.0"
#' data2 <- system.file("extdata/bds_v3.0/smocc/Laura_S.json",
#'   package = "jamesdemodata")
#' q <- read_bds(data2)
#' q
#'
#' # Equivalent, but specifying the built-in schema file bds_v3.0.json
#' schema2 <- system.file("schemas/bds_v3.0.json", package = "bdsreader")
#' r <- read_bds(data2, schema = schema2)
#' identical(q, r)
#'
#' # Automatic detection of format 3.0
#' # s <- read_bds(data2)
#' # identical(q, s)
#'
#' # Reading data with older format (bds_v1.0)
#' data1 <- system.file("extdata/bds_v1.0/smocc/Laura_S.json",
#'   package = "jamesdemodata")
#' t <- read_bds(data1)
#' t
#'
#' # same, but using a built-in schema file
#' schema1 <- system.file("schemas/bds_v1.0.json", package = "bdsreader")
#' u <- read_bds(data1, schema = schema1)
#' identical(t, u)
#' @export
read_bds <- function(txt = NULL,
                     auto_format = TRUE,
                     format = "1.0",
                     schema = NULL,
                     validate = FALSE,
                     append_ddi = FALSE,
                     intermediate = FALSE,
                     verbose = FALSE,
                     ...) {
  # Step 1: return empty target if needed
  if (is.null(txt)) {
    return(make_target(NULL))
  }

  # Step 2: read js object
  txt <- txt[1L]
  if (jsonlite::validate(txt)) {
    js <- txt
  } else {
    err <- rlang::catch_cnd({
      js <- readr::read_lines(file = txt)
    })
    if (!is.null(err)) {
      message("Cannot read 'txt': ", txt)
      return(make_target(NULL))
    }
  }

  # Step 3: convert JSON into R raw list
  err <- rlang::catch_cnd({
    raw <- fromJSON(js)
  })
  if (!is.null(err)) {
    message(conditionMessage(err))
    return(make_target(NULL))
  }

  # Step 4: define schema
  dfmt <- raw$Format[1]
  format <- ifelse(auto_format && !is.null(dfmt), dfmt, format)
  schema_list <- set_schema(format, schema)
  format <- schema_list$format
  schema <- schema_list$schema
  if (!file.exists(schema)) {
    stop("Schema file ", schema, " not found.")
  }

  # Step 5: optionally, perform schema validation
  if (validate) {
    res <- jsonvalidate::json_validate(js, schema, engine = "ajv",
                                       verbose = TRUE)
    msg <- parse_valid(res)

    if (length(msg$required) > 0L) {
      if (any(grepl("required", msg$required)) ||
          any(grepl("verplicht", msg$required)) ||
          any(grepl("should", msg$required))) {
        throw_messages(msg$required)
      }
    }
    throw_messages(msg$supplied)
  }

  # Step 6: convert raw to R object
  major <- as.integer(substr(format, 1L, 1L))
  if (major %in% c(1, 2)) {
    bds <- NULL
  } else {
    bds <- convert_raw_df(raw)
  }

  # Step 7: report on manual range checks
  if (major %in% c(1, 2)) {
    ranges <- check_ranges_12(raw, major)
  } else {
    bds <- check_ranges_3(bds)
  }

  # Step 8: convert ddi, calculate D-score
  if (major %in% c(1, 2)) {
    ddi <- convert_ddi_gsed_12(raw, ranges, major)
    ds <- dscore(data = ddi, key = "gsed2212")
  } else {
    ddi <- convert_ddi_gsed_3(bds)
    ds <- ddi %>%
      pivot_wider(names_from = "lex_gsed", values_from = c("pass")) %>%
      dscore(key = "gsed2212")
  }

  # Step 9: parse to list with components: psn, xy
  if (major %in% c(1, 2)) {
    x <- convert_checked_list_12(raw, ranges, append_ddi = append_ddi,
                                 format = format, ds = ds)
  } else {
    x <- convert_checked_list_3(bds, ds)
  }

  ## Step 10: append DDI
  if (major %in% c(1, 2)) {
    if (nrow(ddi) && append_ddi) {
      x$xy <- bind_rows(
        x$xy,
        ddi %>%
          pivot_longer(
            cols = -all_of("age"), names_to = "yname",
            values_to = "y", values_drop_na = TRUE,
            values_transform = list(y = as.numeric)
          ) %>%
          mutate(
            xname = "age",
            x = .data$age
          )
      )
    }
  }

  # Step 11: add Z-scores, analysis metric for three-letter ynames
  xyz <- x$xy %>%
    mutate(
      sex = (!!x)$psn$sex,
      ga = (!!x)$psn$ga
    ) %>%
    mutate(
      zref = set_refcodes(.),
      zref = ifelse(nchar(.data$yname) == 3L, .data$zref, NA_character_),
      zname = ifelse(nchar(.data$yname) == 3L, paste0(.data$yname, "_z"),
                     NA_character_),
      z = y2z(
        y = .data$y,
        x = .data$x,
        refcode = .data$zref,
        pkg = "nlreferences",
        verbose = verbose
      )
    ) %>%
    select(all_of(c("age", "xname", "yname", "zname", "zref", "x", "y", "z")))

  # Step 12: write intermediate result for later use
  if (major >= 3 && intermediate) {
    jsonlite::write_json(raw, "input.json")
    jsonlite::write_json(bds, "bds.json")
    jsonlite::write_json(ddi, "ddi.json")
    jsonlite::write_json(x$xy, "xy.json")
    jsonlite::write_json(x$psn, "psn.json")
  }

  # Step 13: return primary analysis object in JAMES internal format
  obj <- list(psn = x$psn, xyz = xyz)
  return(obj)
}
