#' Initialize a bdsreader object
#'
#' Constructs a standardized data object for use with bdsreader-compatible tools.
#' Returns a list with personal and measurement data. If `psn` or `xyz` are not provided,
#' default templates are created.
#'
#' @param psn A tibble or data frame containing subject-level information. If `NULL`, a template is returned.
#' @param xyz A tibble or data frame with long-format measurement data. If `NULL`, an empty template is returned.
#'
#' @return A list of class `"bdsreader"` with two elements:
#' \describe{
#'   \item{psn}{A tibble with one row per subject. See details below.}
#'   \item{xyz}{A tibble in long format with age-based measurements. See details below.}
#' }
#'
#' @section Fields in `psn`:
#' \describe{
#'   \item{id}{Integer. Unique subject ID. Set to -1 if not provided.
#'   Code id == 0 is the current target child}
#'   \item{name}{Character. Child name or alias.}
#'   \item{dob}{Date. Date of birth of the child.}
#'   \item{dobm}{Date. Date of birth of the mother.}
#'   \item{dobf}{Date. Date of birth of the father.}
#'   \item{src}{Character. Data source or study identifier.}
#'   \item{dnr}{Character. Donor or provenance code.}
#'   \item{sex}{Character. Sex of the child (e.g., "male", "female").}
#'   \item{gad}{Numeric. Gestational age in days.}
#'   \item{ga}{Numeric. Gestational age in completed weeks.}
#'   \item{smo}{Numeric. Smoking status during pregnancy (0 or 1).}
#'   \item{bw}{Numeric. Birth weight in grams.}
#'   \item{hgtm}{Numeric. Height of mother in centimeters.}
#'   \item{hgtf}{Numeric. Height of father in centimeters.}
#'   \item{agem}{Numeric. Age of mother at birth in years.}
#'   \item{etn}{Character. Ethnicity code.}
#'   \item{pc4}{Character. Postal code (NL-style 4-digit format).}
#'   \item{blbf}{Integer. Birth land biological father (landcode).}
#'   \item{blbm}{Integer. Birth land biological mother (landcode).}
#'   \item{eduf}{Integer. Educational level of father.}
#'   \item{edum}{Integer. Educational level of mother.}
#'   \item{par}{Integer. Parity (number of previous children).}
#' }
#'
#' @section Fields in `xyz`:
#' \describe{
#'   \item{age}{Numeric. Age at measurement (e.g., in days or months).}
#'   \item{xname}{Character. Name of the x-variable (typically "age").}
#'   \item{yname}{Character. Name of the measurement variable (e.g., "hgt").}
#'   \item{zname}{Character. Name of the z-score variable. (e.g. "hgt_z")}
#'   \item{zref}{Character. Reference standard for z-score (e.g., "nl_2010_hgt_male_").}
#'   \item{x}{Numeric. Value of the x-variable.}
#'   \item{y}{Numeric. Observed measurement value.}
#'   \item{z}{Numeric. Standardized z-score.}
#' }
#'
#' @examples
#' bds <- init_bdsreader()
#' validate_bdsreader(bds)
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
      eduf = NA_integer_,
      edum = NA_integer_,
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
