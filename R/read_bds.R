#' Reads selected BDS data of a child
#'
#' This function takes data from a json source, calcuates the D-score,
#' calculates Z-scores and transforms the data as a tibble.
#' @param txt A JSON string, URL or file
#' @param schema A JSON string, URL or file that selects the JSON validation
#' schema.
#' @param append_ddi Should DDI measures be appended?
#' @param verbose Show warnings of missing references
#' @param \dots Additional parameter passed down to
#' @return A `tbl_df` object with 8 columns and a `child` attribute
#' @author Stef van Buuren 2021
#' @seealso [jsonlite::fromJSON()]
#' @examples
#' fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
#' q <- read_bds(fn)
#' q
#' @export
read_bds <- function(txt = NULL, schema = NULL,
                     append_ddi = FALSE, verbose = FALSE, ...) {
  if (is.null(txt)) {
    xyz <- tibble(
      xname = character(0),
      yname = character(0),
      zname = character(0),
      x = numeric(0),
      y = numeric(0),
      z = numeric(0),
      age = numeric(0),
      refcode_z = character(0))
    attr(xyz, "child") <- tibble(
      id = -1L,
      name = NA_character_,
      dob = as.Date(NA),
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

  # Check. Tranform json errors (e.g. no file, invalid json) into a
  # warning, and exit with empty target object.
  checked <- tryCatch(
    expr = verify(txt, schema = schema, ...),
    error = function(cnd) {
      stop(conditionMessage(cnd))
    }
  )

  # parse to list with child/time components
  x <- convert_checked_list(checked, append_ddi = append_ddi)

  # add Z-scores, analysis metric
  xyz <- x$xy %>%
    mutate(
      sex = (!!x)$child$sex,
      ga = (!!x)$child$ga
    ) %>%
    mutate(
      refcode_z = set_refcodes(.),
      refcode_z = ifelse(nchar(.data$yname) == 3L, .data$refcode_z, NA_character_),
      zname = ifelse(nchar(.data$yname) == 3L, paste0(.data$yname, "_z"), NA_character_),
      z = y2z(
        y = .data$y,
        x = .data$x,
        refcode = .data$refcode_z,
        pkg = "nlreferences",
        verbose = verbose
      )
    ) %>%
    select(all_of(c("xname", "yname", "zname", "x", "y", "z", "age", "refcode_z")))

  attr(xyz, "child") <- x$child
  xyz
}
