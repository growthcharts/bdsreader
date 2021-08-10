#' Reads selected BDS data of a person
#'
#' This function takes data from a json source, validates the contents against a
#' JSON validation schema, perform checks, calculates the D-score, calculates
#' Z-scores and transforms the data as a tibble with a `person` attribute.
#' @param txt A JSON string, URL or file
#' @param auto_format Logical. Should the format be read from the data? Default is `TRUE`.
#' @param append_ddi Should the DDI responses be appended?
#' @param verbose Show verbose output for [centile::y2z()]
#' @param \dots Passed down to [jsonlite::fromJSON()]
#' @inheritParams set_schema
#' @return A tibble with 8 columns with a `person` attribute
#' @author Stef van Buuren 2021
#' @details
#' If `txt` is unspecified or `NULL`, then the function return will have zero rows.
#'
#' The `format` and `schema` arguments specify the format of the JSON input
#' data argument `txt`. The default is `format = "1.0"` expects that the JSON
#' input data conform to the schema specified in
#' `system.file("schemas/bds_v1.0.json", package = "bdsreader")`. The alternative
#' is `format = 2.0` expects data coded according to
#' `system.file("schemas/bds_v2.0.json", package = "bdsreader")`. For new users,
#' we recommend format `"2.0"`.
#'
#' Alternatively, the format can be specified in the JSON data file with an entry
#' named `Format`. For `auto_format == TRUE`, the data specification overrides
#' any `format` and `schema` arguments to the `read_bds()` function.
#' The schema `bds_v2.0.json` schema requires
#' the `Format` field, so the correct format is automatically set by the data.
#'
#' If you erroneously read a JSON file of format `"1.0"` using format `"2.0"`
#' you may see an error:
#' `Error in b[b$ElementNummer == f & !is.na(b$ElementNummer), "Waarde"] : incorrect number of dimensions`.
#' In that make sure that you are reading with the `format = "1.0"` argument.
#' Reversely, if you erroneously read a JSON file of format `"2.0"` using format
#' `"1.0"` you may see messages `.ClientGegevens should be object` and
#' `Missing 'ClientGegevens$Groepen'`. In that case, specify `format = "2.0"`.
#'
#' @seealso [jsonlite::fromJSON()], [centile::y2z()]
#' @examples
#' # Assume that jamesdemodata is installed locally.
#' # If not use remotes::install_github("growthcharts/jamesdemodata")
#'
#' # Read file with input data according to format "2.0".
#' data2 <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
#' q <- read_bds(data2, format = "2.0")
#' q
#'
#' # Equivalent, but specifying the built-in schema file bds_v2.0.json
#' schema2 <- system.file("schemas/bds_v2.0.json", package = "bdsreader")
#' r <- read_bds(data2, schema = schema2)
#' identical(q, r)
#'
#' # Automatic detection of format 2.0
#' # s <- read_bds(data2)
#' # identical(q, s)
#'
#' # Reading data with older format (bds_v1.0)
#' data1 <- system.file("extdata/bds_v1.0/smocc/Laura_S.json", package = "jamesdemodata")
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
                     append_ddi = FALSE,
                     verbose = FALSE,
                     ...) {
  if (is.null(txt)) {
    xyz <- tibble(
      age = numeric(0),
      xname = character(0),
      yname = character(0),
      zname = character(0),
      zref = character(0),
      x = numeric(0),
      y = numeric(0),
      z = numeric(0))
    attr(xyz, "persondata") <- tibble(
      id = -1L,
      name = NA_character_,
      dob = as.Date(NA),
      dobm = as.Date(NA),
      dobf = as.Date(NA),
      src = NA_character_,
      dnr = NA_character_,
      sex = NA_character_,
      gad = NA_real_,
      ga = NA_real_,
      smo = NA_real_,
      bw = NA_real_,
      hgtm = NA_real_,
      hgtf = NA_real_,
      agem = NA_real_,
      etn = NA_character_)
    return(xyz)
  }

  if (txt == "") {
    stop("Argument txt is an empty string.")
  }

  # Check. Tranform json errors (e.g. no file, invalid json) into a
  # warning, and exit with empty target object.
  checked <- tryCatch(
    expr = verify(txt, auto_format, format, schema, ...),
    error = function(cnd) {
      stop(conditionMessage(cnd))
    }
  )

  # parse to list with components: persondata, xy
  x <- convert_checked_list(checked, append_ddi = append_ddi,
                            format = checked$schema_list$format)

  # add Z-scores, analysis metric
  # try to find a reference only if yname has three letters
  xyz <- x$xy %>%
    mutate(
      sex = (!!x)$persondata$sex,
      ga = (!!x)$persondata$ga
    ) %>%
    mutate(
      zref = set_refcodes(.),
      zref = ifelse(nchar(.data$yname) == 3L, .data$zref, NA_character_),
      zname = ifelse(nchar(.data$yname) == 3L, paste0(.data$yname, "_z"), NA_character_),
      z = y2z(
        y = .data$y,
        x = .data$x,
        refcode = .data$zref,
        pkg = "nlreferences",
        verbose = verbose
      )
    ) %>%
    select(all_of(c("age", "xname", "yname", "zname", "zref", "x", "y", "z")))

  attr(xyz, "person") <- x$persondata
  xyz
}
