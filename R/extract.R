extract_dob <- function(d, which = "00") {

  if (which == "0") {
    b <- d$ClientGegevens$Elementen
    dob <- ymd(b[b$Bdsnummer == 20, 2])
    if (!length(dob)) {
      return(as.Date(NA))
    }
    return(dob)
  }

  p <- d$ClientGegevens$Groepen[[1]]
  if (is.null(p)) {
    return(return(as.Date(NA)))
  }
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62L, "Waarde"]
    if (parent == which) {
      dobp <- ymd(pp[pp$Bdsnummer == 63L, 2L])
      if (!length(dobp)) {
        return(as.Date(NA))
      }
      return(dobp)
    }
  }
  as.Date(NA)
}

extract_sex <- function(b) {
  s <- b[b$Bdsnummer == 19L, 2L]
  if (length(s) == 0L) {
    return(NA_character_)
  }
  switch(s,
         "1" = "male",
         "2" = "female",
         NA_character_
  )
}

extract_agep <- function(dob, dobp) {
  # returns age of parent in completed years
  agep <- as.numeric(trunc(difftime(dob, dobp, "days") / 365.25))
  if (!length(agep)) agep <- NA_real_
  agep
}

extract_field <- function(d, f = 245L) {
  z <- d$Contactmomenten[[2L]]
  as.numeric(unlist(lapply(z, function(x, f2 = f) {
    ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA)
  })))
}

extract_field2 <- function(d, f, l1, l2) {
  b <- d[[l1]][[l2]]
  v <- b[b$Bdsnummer == f, "Waarde"]
  ifelse(!length(v), NA_real_, v)
}

extract_field3 <- function(d, f, l1, l2, l3, which_parent = "02") {
  p <- d[[l1]][[l2]][[l3]]
  if (is.null(p)) {
    return(NA)
  }
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62L, "Waarde"]
    if (parent == which_parent) {
      return(pp[pp$Bdsnummer == f, 2L])
    }
  }
}
