extract_dob <- function(d, which = "00", v = 1) {
  if (which == "00") {
    b <- switch(v,
                d$ClientGegevens$Elementen,
                d$ClientGegevens,
                d$clientDetails
    )
    if (!length(b)) {
      return(as.Date(NA))
    }
    dob <- switch(v,
                  ymd(b[b$Bdsnummer == 20, 2]),
                  ymd(b[b$ElementNummer == 20 & !is.na(b$ElementNummer), 2]),
                  ymd(b[b$bdsNumber == 20 & !is.na(b$bdsNumber), 2])
    )
    if (!length(dob)) {
      return(as.Date(NA))
    }
    return(dob)
  }

  p <- switch(v,
              d$ClientGegevens$Groepen[[1]],
              d$ClientGegevens$GenesteElementen,
              d$nestedDetails
  )
  if (!length(p)) {
    return(return(as.Date(NA)))
  }

  if (v == 3) p <- split(p, f = seq_len(nrow(p)))
  p <- p[sapply(p, length) > 0]
  for (i in seq_along(p)) {
    pp <- p[[i]]
    parent <- switch(v,
                     pp[pp$Bdsnummer == 62L, "Waarde"],
                     pp[pp$ElementNummer == 62L, "Waarde"],
                     pp[pp$nestingBdsNumber == 62L, "nestingCode"]
    )
    if (is.null(parent)) next
    if (parent == which) {
      # for v3 the parent codes are 'NFTH' (natural father) and "NMTH" (natural
      # mother)
      dobp <- switch(
        v,
        ymd(pp[pp$Bdsnummer == 63L, 2L]),
        ymd(pp[pp$ElementNummer == 63L, 2L]),
        ymd(pp$clientDetails[[1]][pp$clientDetails[[1]]$bdsNumber == 63L, 2L])
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
  s <- switch(v,
              b[b$Bdsnummer == 19L, 2L],
              b[b$ElementNummer == 19L & !is.na(b$ElementNummer), 2L],
              b[b$bdsNumber == 19L & !is.na(b$bdsNumber), 2L]
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
  if (v == 1) {
    z <- d$Contactmomenten[[2L]]
    as.numeric(unlist(lapply(z, function(x, f2 = f) {
      ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA)
    }
    )))
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
    v <- ifelse(v == "NA", NA_real_, v)
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
  p <- switch(v,
              d[["ClientGegevens"]][["Groepen"]][["Elementen"]],
              d[["ClientGegevens"]][["GenesteElementen"]],
              d[["nestedDetails"]])
  if (length(p) == 0) {
    return(NA)
  }
  if (v == 3) p <- split(p, f = seq_len(nrow(p)))
  for (i in seq_along(p)) {
    pp <- p[[i]]
    parent <- switch(v,
                     pp[pp$Bdsnummer == 62L, "Waarde"],
                     pp[pp$ElementNummer == 62L, "Waarde"],
                     pp[pp$nestingBdsNumber == 62L, "nestingCode"])
    if (is.null(parent)) next
    if (parent == which_parent) {
      wp <- switch(
        v,
        pp[pp$Bdsnummer == f, 2L],
        pp[pp$ElementNummer == f, 2L],
        pp$clientDetails[[1]][pp$clientDetails[[1]]$bdsNumber == f, 2L])
      return(wp)
    }
  }
  return(NA)
}

measurements_to_df <- function(cm) {
  # convert clientMeasurement list components
  # into data.frame with bds, date, value
  # convert all values to integer
  for (i in seq_along(cm$values)) {
    cm$values[[i]]$value <- as.integer(cm$values[[i]]$value)
  }
  bind_cols(bds = rep(cm$bdsNumber, sapply(cm$values, nrow)),
            bind_rows(cm$values))
}
