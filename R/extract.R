extract_dob <- function(d, which = "00", v = 1) {
  if (which == "00") {
    switch(v,
           b <- d$ClientGegevens$Elementen,
           b <- d$ClientGegevens,
           b <- d$clientDetails
    )
    if (!length(b)) {
      return(as.Date(NA))
    }
    switch(v,
           dob <- ymd(b[b$Bdsnummer == 20, 2]),
           dob <- ymd(b[b$ElementNummer == 20 & !is.na(b$ElementNummer), 2]),
           dob <- ymd(b[b$bdsNumber == 20 & !is.na(b$bdsNumber), 2])
    )
    if (!length(dob)) {
      return(as.Date(NA))
    }
    return(dob)
  }

  switch(v,
         p <- d$ClientGegevens$Groepen[[1]],
         p <- d$ClientGegevens$GenesteElementen,
         p <- d$nestedDetails
  )
  if (!length(p)) {
    return(return(as.Date(NA)))
  }

  if (v == 3) p <- split(p, f = (1:nrow(p)))
  p <- p[sapply(p, length) > 0]
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    switch(v,
           parent <- pp[pp$Bdsnummer == 62L, "Waarde"],
           parent <- pp[pp$ElementNummer == 62L, "Waarde"],
           parent <- pp[pp$nestingBdsNumber == 62L, "nestingCode"]
           )
    if (is.null(parent)) next
    if (parent == which) {
      # for v3 the parent codes are 'NFTH' (natural father) and "NMTH" (natural
      # mother)
      switch(v,
             dobp <- ymd(pp[pp$Bdsnummer == 63L, 2L]),
             dobp <- ymd(pp[pp$ElementNummer == 63L, 2L]),
             dobp <- ymd(pp$clientDetails[[1]][pp$clientDetails[[1]]$bdsNumber == 63L, 2L])
      )
      if (!length(dobp)) {
        return(as.Date(NA))
      }
      return(dobp)
    }
  }
  as.Date(NA)
}


extract_sex <- function(b, v = 1) {

  if (length(b) == 0L) {
    return(NA_character_)
  }
  switch(v,
         s <- b[b$Bdsnummer == 19L, 2L],
         s <- b[b$ElementNummer == 19L & !is.na(b$ElementNummer), 2L],
         s <- b[b$bdsNumber == 19L & !is.na(b$bdsNumber), 2L]
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
extract_field <- function(d, f = 245L, v = 1) {
  if (v == 1){
    z <- d$Contactmomenten[[2L]]
    as.numeric(unlist(lapply(z, function(x, f2 = f) {
      ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA)
    })))
  } else if (v == 2) {
    z <- d$ContactMomenten[[2L]]
    as.numeric(unlist(lapply(z, function(x, f2 = f) {
      ifelse("Waarde" %in% names(x),
             x[x$ElementNummer == f2 & !is.na(x$ElementNummer), "Waarde"], NA)
    })))
  } else if (v == 3) {
    f2 <- d$clientMeasurements[[1L]]
    z <- d$clientMeasurements[[2L]]
    as.data.frame(z[f2 == f])
  }
}


# For ClientGegevens
extract_field2 <- function(d, f, v = 1) {
  if (v == 1) {
    b <- d[["ClientGegevens"]][["Elementen"]]
    if (!length(b)) return(NA_real_)
    v <- b[b$Bdsnummer == f, "Waarde"]
  } else if (v == 2) {
    b <- d[["ClientGegevens"]]
    if (!length(b)) return(NA_real_)
    v <- b[b$ElementNummer == f & !is.na(b$ElementNummer), "Waarde"]
  } else if (v == 3) {
    b <- d[["clientDetails"]]
    if (!length(b)) return(NA_real_)
    v <- b[b$bdsNumber == f & !is.na(b$bdsNumber), "value"]
  }
  ifelse(!length(v), NA_real_, v)
}

# For parent data
# FIXME currently only works for nested clientDetails, not clientMeasurements
extract_field3 <- function(d, f, which_parent = "02", v = 1) {
  switch(v,
         p <- d[["ClientGegevens"]][["Groepen"]][["Elementen"]],
         p <- d[["ClientGegevens"]][["GenesteElementen"]],
         p <- d[["nestedDetails"]])
  if (length(p) == 0) {
    return(NA)
  }
  if (v == 3) p <- split(p, f = (1:nrow(p)))
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    switch(v,
           parent <- pp[pp$Bdsnummer == 62L, "Waarde"],
           parent <- pp[pp$ElementNummer == 62L, "Waarde"],
           parent <- pp[pp$nestingBdsNumber == 62L, "nestingCode"])
    if (is.null(parent)) next
    if (parent == which_parent) {
      switch(v,
             return(pp[pp$Bdsnummer == f, 2L]),
             return(pp[pp$ElementNummer == f, 2L]),
             return(pp$clientDetails[[1]][pp$clientDetails[[1]]$bdsNumber == f, 2L]))
    }
  }
  return(NA)
}
