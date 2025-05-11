#' Validate a bdsreader object
#'
#' Checks structure, required columns, and column types for both `psn` and `xyz`.
#'
#' @param obj An object of class `bdsreader`.
#' @param strict Logical. If `TRUE`, stops on first error. If `FALSE`, returns a character vector of issues.
#'
#' @return TRUE if valid, otherwise a vector of issues or an error.
#' @examples
#' bds <- init_bdsreader()
#' validate_bdsreader(bds)  # TRUE
#'
#' # Corrupt type
#' bds$psn$gad <- as.character(bds$psn$gad)
#' validate_bdsreader(bds)
# "Column 'gad' in 'psn' is of type character, expected numeric"
#' @export
validate_bdsreader <- function(obj, strict = FALSE) {
  issues <- character()

  # Class check
  if (!inherits(obj, "bdsreader")) {
    issues <- c(issues, "Object does not inherit from class 'bdsreader'.")
    if (strict) stop(issues, call. = FALSE)
  }

  ## Define expected structure
  psn_schema <- list(
    id = "integer", name = "character", dob = "Date", dobm = "Date", dobf = "Date",
    src = "character", dnr = "character", sex = "character",
    gad = "numeric", ga = "numeric", smo = "numeric", bw = "numeric",
    hgtm = "numeric", hgtf = "numeric", agem = "numeric", etn = "character",
    pc4 = "character", blbf = "integer", blbm = "integer",
    eduf = "integer", edum = "integer", par = "integer"
  )

  xyz_schema <- list(
    age = "numeric", xname = "character", yname = "character",
    zname = "character", zref = "character",
    x = "numeric", y = "numeric", z = "numeric"
  )

  ## Validate psn
  if (!"psn" %in% names(obj)) {
    issues <- c(issues, "'psn' component is missing.")
  } else {
    psn <- obj$psn
    for (col in names(psn_schema)) {
      if (!col %in% names(psn)) {
        issues <- c(issues, paste0("Missing column in 'psn': ", col))
      } else {
        expected <- psn_schema[[col]]
        actual <- class(psn[[col]])[1]
        if (expected == "numeric") {
          valid <- actual %in% c("numeric", "double")
        } else {
          valid <- identical(actual, expected)
        }
        if (!valid) {
          issues <- c(issues, paste0("Column '", col, "' in 'psn' is of type ", actual, ", expected ", expected))
        }
      }
    }
  }

  ## Validate xyz
  if (!"xyz" %in% names(obj)) {
    issues <- c(issues, "'xyz' component is missing.")
  } else {
    xyz <- obj$xyz
    for (col in names(xyz_schema)) {
      if (!col %in% names(xyz)) {
        issues <- c(issues, paste0("Missing column in 'xyz': ", col))
      } else {
        expected <- xyz_schema[[col]]
        actual <- class(xyz[[col]])[1]
        if (expected == "numeric") {
          valid <- actual %in% c("numeric", "double")
        } else {
          valid <- identical(actual, expected)
        }
        if (!valid) {
          issues <- c(issues, paste0("Column '", col, "' in 'xyz' is of type ", actual, ", expected ", expected))
        }
      }
    }
  }

  ## Return or raise
  if (length(issues) == 0L) {
    return(TRUE)
  } else if (strict) {
    stop(paste(issues, collapse = "\n"), call. = FALSE)
  } else {
    return(issues)
  }
}
