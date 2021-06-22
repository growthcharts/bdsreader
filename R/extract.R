extract_dob <- function(d, which = "00", version = 1) {
  if (which == "00") {
    switch(version,
           b <- d$ClientGegevens$Elementen,
           b <- d$ClientGegevens
    )
    if (!length(b)) {
      return(as.Date(NA))
    }
    switch(version,
           dob <- ymd(b[b$Bdsnummer == 20, 2]),
           dob <- ymd(b[b$ElementNummer == 20 & !is.na(b$ElementNummer), 2])
    )
    if (!length(dob)) {
      return(as.Date(NA))
    }
    return(dob)
  }

  switch(version,
         p <- d$ClientGegevens$Groepen[[1]],
         p <- d$ClientGegevens$GenesteElementen
  )
  if (!length(p)) {
    return(return(as.Date(NA)))
  }
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    switch(version,
           parent <- pp[pp$Bdsnummer == 62L, "Waarde"],
           parent <- pp[pp$ElementNummer == 62L, "Waarde"]
           )
    if (is.null(parent)) next
    if (parent == which) {
      switch(version,
             dobp <- ymd(pp[pp$Bdsnummer == 63L, 2L]),
             dobp <- ymd(pp[pp$ElementNummer == 63L, 2L])
      )
      if (!length(dobp)) {
        return(as.Date(NA))
      }
      return(dobp)
    }
  }
  as.Date(NA)
}


extract_sex <- function(b, version = 1) {

  if (length(b) == 0L) {
    return(NA_character_)
  }
  switch(version,
         s <- b[b$Bdsnummer == 19L, 2L],
         s <- b[b$ElementNummer == 19L & !is.na(b$ElementNummer), 2L]
  )
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

extract_agep_v1 <- function(dob, dobp) {
  # returns age of parent in completed years
  agep <- as.numeric(trunc(difftime(dob, dobp, "days") / 365.25))
  if (!length(agep)) agep <- NA_real_
  agep
}

extract_agep_v2 <- function(dob, dobp) {
  # returns age of parent in completed years
  agep <- as.numeric(trunc(difftime(dob, dobp, "days") / 365.25))
  if (!length(agep)) agep <- NA_real_
  agep
}

# For ContactMomenten
extract_field <- function(d, f = 245L, version = 1) {
  if (version == 1){
    z <- d$Contactmomenten[[2L]]
    as.numeric(unlist(lapply(z, function(x, f2 = f) {
      ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA)
    })))
  } else if (version == 2) {
    z <- d$ContactMomenten[[2L]]
    as.numeric(unlist(lapply(z, function(x, f2 = f) {
      ifelse("Waarde" %in% names(x),
             x[x$ElementNummer == f2 & !is.na(x$ElementNummer), "Waarde"], NA)
    })))
  }
}

# For ClientGegevens
extract_field2 <- function(d, f, version = 1) {
  if (version == 1) {
    b <- d[["ClientGegevens"]][["Elementen"]]
    if (!length(b)) return(NA_real_)
    v <- b[b$Bdsnummer == f, "Waarde"]
  } else if (version == 2) {
    b <- d[["ClientGegevens"]]
    if (!length(b)) return(NA_real_)
    v <- b[b$ElementNummer == f & !is.na(b$ElementNummer), "Waarde"]
  }
  ifelse(!length(v), NA_real_, v)
}

# For parent data
extract_field3 <- function(d, f, which_parent = "02", version = 1) {
  switch(version,
         p <- d[["ClientGegevens"]][["Groepen"]][["Elementen"]],
         p <- d[["ClientGegevens"]][["GenesteElementen"]])
  if (length(p) == 0) {
    return(NA)
  }
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    switch(version,
           parent <- pp[pp$Bdsnummer == 62L, "Waarde"],
           parent <- pp[pp$ElementNummer == 62L, "Waarde"])
    if (is.null(parent)) next
    if (parent == which_parent) {
      switch(version,
             return(pp[pp$Bdsnummer == f, 2L]),
             return(pp[pp$ElementNummer == f, 2L]))
    }
  }
  return(NA)
}
