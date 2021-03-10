#' Get the date of birth of an individual
#'
#' @param ind An object of class \code{individual}.
#' @return  Object of class \code{Date}. If dob is missing,
#' the function return dob 01-01-2000.
#' @examples
#' # Extract date of birth
#' #ind <- new("individual", dob = Sys.Date(),
#' #            hgt = new("xyz", x = c(0.3, 2, 6)),
#' #           wgt = new("xyz", x = c(0.2, 1), y = c(1, NA)))
#' #get_dob(ind)
#' @export
get_dob <- function(ind) {
  if (is.na(slot(ind, "dob")))
    return(as.Date("01-01-00", format = "%d-%m-%y"))
  slot(ind, "dob")
}
