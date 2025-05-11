#' Initialize a bdsreader-like object
#'
#' This function constructs a list containing `psn` (personal info) and `xyz`
#' (measurement data), following the expected format for bdsreader workflows.
#'
#' @param psn A data frame with personal information, one row per subject. If NULL, a default template is created.
#' @param xyz A data frame with longitudinal measurement data. If NULL, a default empty template is created.
#'
#' @return An object of class `bdsreader`, a list with components:
#'   \describe{
#'     \item{psn}{A data frame with demographic and parental information.}
#'     \item{xyz}{A data frame with age-based longitudinal measurements.}
#'   }
#'
#' @export
init_bdsreader <- function(psn = NULL, xyz = NULL) {
  if (is.null(psn) || is.null(xyz)) {
    psn <- tibble(
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
      etn = NA_character_,
      pc4 = NA_character_,
      blbf = NA_integer_,
      blbm = NA_integer_,
      eduf = NA_character_,
      edum = NA_character_,
      par = NA_integer_)
    xyz <- tibble(
      age = numeric(0),
      xname = character(0),
      yname = character(0),
      zname = character(0),
      zref = character(0),
      x = numeric(0),
      y = numeric(0),
      z = numeric(0))
  }
  obj <- list(psn = psn, xyz = xyz)
  class(obj) <- "bdsreader"
  return(obj)
}
