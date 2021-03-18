#' Convert json BDS data for single individual to target object
#'
#' This function takes data from a json source and saves it as a an object
#' of class `target`.
#' @param txt a JSON string, URL or file
#' @param schema A JSON string, URL or file that selects the JSON validation
#' schema.
#' @param append_ddi Should DDI measures be appended?
#' @param verbose Show warnings of missing references
#' @param \dots Additional parameter passed down to
#' @return An object of class `target` or `NULL`
#' @author Stef van Buuren 2021
#' @seealso [jsonlite::fromJSON()]
#' @examples
#' fn <- system.file("extdata", "allegrosultum", "client3.json", package = "jamestest")
#' q <- convert_bds_target(fn)
#' @export
convert_bds_target <- function(txt = NULL, schema = NULL,
                               append_ddi = FALSE, verbose = FALSE, ...) {
  if (is.null(txt)) {
    return(NULL)
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
  x <- convert_checked_list(checked, ...)

  # add Z-scores, analysis metric
  xyz <- x$xy %>%
    mutate(
      sex = (!!x)$child$sex,
      ga = (!!x)$child$ga
    ) %>%
    mutate(
      refcode_z = nlreferences::set_refcodes(.),
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
