#' Save donordata format as JSON
#'
#' Converts data in `donordata` format to a JSON file per child.
#' @param data Dataset in donordata format (having child and time elements)
#' @param ids Integer vector with the id number for the individuals
#' @param path Optional. Path where files should be written. By default, `path` is the
#' working directory. The function creates the path if it doesn't already exist.
#' @param names Optional. Character vector with `length(ids)` elements used to
#' construct the file name. Spaces are replaced by underscores. If not specified,
#' the function uses the `ids` vector to create file names.
#' @param indent Optional. Integer. Number of spaces to indent when using
#' `jsonlite::prettify()`. When not specified, the function writes minified json.
#' @param \dots Forwarded to [bdsreader::write_bds()]
#' @note
#' If child birth date is unknown, then 2000-01-01 is used.
#' Birth date of the mother is back-calculated from `agem`, and thus imprecise.
#' @author Stef van Buuren 2021
#' @examples
#' names <- c("Laura S", "Thomas S")
#' ids <- as.integer(c(34071, 34072))
#' \dontrun{
#' export_as_bds(minidata, ids = ids, names = names, indent = 2)
#' }
#' @export
export_as_bds <- function(data,
                          ids,
                          path = NULL,
                          names = NULL,
                          indent = NULL,
                          ...) {
  if (!is.null(names) && length(ids) != length(names)) {
    stop("Incompatible lengths of `ids` and `names`.")
  }

  # fix missing/unknown fields
  if (!hasName(data$child, "dob"))
    data$child$dob <- as.Date("01-01-00", format = "%d-%m-%y")
  data$child$dob[is.na(data$child$dob)] <- as.Date("01-01-00", format = "%d-%m-%y")
  if (!hasName(data$child, "name")) {
    data$child$name <- names[order(ids)]
  } else {
    rn <- names[order(ids)]
    data$child$name[is.na(data$child$name)] <- rn[is.na(data$child$name)]
  }
  if (!hasName(data$child, "hgtm")) data$child$hgtm <- NA_real_
  if (!hasName(data$child, "hgtf")) data$child$hgtf <- NA_real_
  if (!hasName(data$child, "name")) data$child$name <- NA_character_
  if (!hasName(data$child, "agem")) data$child$agem <- NA_real_
  if (!hasName(data$child, "smo")) data$child$smo <- NA_real_
  if (!hasName(data$child, "etn")) data$child$etn <- NA_character_
  if (!hasName(data$child, "bw")) data$child$bw <- NA_real_
  if (!hasName(data$child, "ga")) data$child$ga <- NA_real_
  if (!hasName(data$child, "gad")) data$child$gad <- NA_real_
  if (!hasName(data$child, "dobf")) data$child$dobf <- as.Date(NA)
  if (!hasName(data$child, "dobm")) data$child$dobm <- as.Date(NA)

  # process fixed person data
  persons <- data$child %>%
    mutate(
      id = as.integer(.data$id),
      dob = as.Date(.data$dob, format = "%d-%m-%y"),
      dobf = as.Date(.data$dobf, format = "%d-%m-%y"),
      dobm = as.Date(.data$dobm, format = "%d-%m-%y"),
    ) %>%
    select(all_of(c(
      "src", "id", "name", "dob", "dobf", "dobm", "sex",
      "gad", "ga", "smo", "bw", "hgtm", "hgtf", "agem", "etn"
    )))

  # if we have agem and no dobm, calculate dobm from agem
  idx <- is.na(persons$dobm) & !is.na(persons$agem)
  persons[idx, "dobm"] <- as.Date(persons$dob[idx] - (persons$agem[idx] + 0.5) * 365.25,
                                  format = "%Y%m%d")

  # if we have ga but no gad, calculate gad from ga
  #  idx <- is.na(persons$gad) & !is.na(persons$agem)
  idx <- is.na(persons$gad)
  persons[idx, "gad"] <- persons$ga[idx] * 7 + 3

  # process time-varying data
  times <- data$time %>%
    select(all_of(c("id", "age", "hgt", "wgt", "hdc")), num_range("bds", 600:1500)) %>%
    mutate(
      id = as.integer(.data$id),
      xname = "age",
      x = .data$age
    ) %>%
    pivot_longer(
      cols = all_of(c("hgt", "wgt", "hdc")) | num_range("bds", 600:1500),
      names_to = "yname", values_to = "y", values_drop_na = TRUE
    )

  # set output files
  if (is.null(names)) names <- as.character(ids)
  fns <- file.path(paste(gsub(" ", "_", names), "json", sep = "."))
  if (!is.null(path)) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    fns <- file.path(path, fns)
  }

  # per id, construct target and write
  fn <- 0
  for (i in ids) {
    fn <- fn + 1
    xyz <- times %>%
      filter(.data$id == i)
    psn <- persons %>%
      filter(.data$id == i)
    target <- make_target(psn = psn, xyz = xyz)
      js <- write_bds(x = target, file = fns[fn], indent = indent, ...)
  }
}
