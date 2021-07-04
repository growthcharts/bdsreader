#' Reads selected BDS data of a person
#'
#' This function takes data from a json source, validates the contents against a
#' JSON validation schema, perform checks, calculates the D-score, calculates
#' Z-scores and transforms the data as a tibble with a `person` attribute.
#' @param txt A JSON string, URL or file
#' @param version Integer. JSON schema version number. There are currently two
#'   schemas supported, versions `1` and `2`.
#' @param schema A file name with the JSON validation schema.
#'   The `schema` argument overrides `version`. The function extracts the
#'   character following the string `"_v"` in the name, and overwrites the
#'   `version` argument by the integer representation of the found character.
#' @param append_ddi Should DDI measures be appended?
#' @param verbose Show verbose output for [centile::y2z()]
#' @param \dots Passed down to [jsonlite::fromJSON()]
#' @return A tibble with 8 columns with a `person` attribute
#' @author Stef van Buuren 2021
#' @seealso [jsonlite::fromJSON()], [centile::y2z()]
#' @examples
#' fn <- system.file("examples", "Laura_S2.json", package = "bdsreader")
#' q <- read_bds(fn, version = 1)
#' q
#' @export
read_bds <- function(txt = NULL,
                     version = 2L,
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
  if (is.null(schema)) {
    switch(
      version,
      {
        schema <- system.file("schemas/bds_v1.0.json", package = "bdsreader", mustWork = TRUE)
      },
      {
        schema <- system.file("schemas/bds_v2.0.json", package = "bdsreader", mustWork = TRUE)
      }
    )
  } else {
    # overwrite version by file name extract
    version <- as.integer(substr(strsplit(schema, "_v")[[1]][2], 1, 1))
  }
  if (!file.exists(schema)) {
    stop("File ", schema, " not found.")
  }

  # Check. Tranform json errors (e.g. no file, invalid json) into a
  # warning, and exit with empty target object.
  checked <- tryCatch(
    expr = verify(txt, schema = schema, ...),
    error = function(cnd) {
      stop(conditionMessage(cnd))
    }
  )

  # parse to list with components: persondata, xy
  x <- convert_checked_list(checked, append_ddi = append_ddi, v = version)

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
