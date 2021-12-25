make_target <- function(psn = NULL, xyz = NULL) {
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
        etn = NA_character_)
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
  obj <- list(psn = psn,
              xyz = xyz)
  class(obj) <- "target"
  return(obj)
}

