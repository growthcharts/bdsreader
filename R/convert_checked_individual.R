convert_checked_individual <- function(checked = NULL, ...) {

  d <- checked$data
  b <- d$ClientGegevens$Elementen
  r <- checked$ranges

  # convert ddi, calculate D-score
  ddi <- convert_ddi_gsed(d, r)
  ds <- dscore(data = ddi, key = "dutch")

  # browser()
  # is this child or message number?
  src <- as.character(d$OrganisatieCode)
  src <- ifelse(length(src), src, "")
  pid <- new("individualID",
             id = 0L,
             name = as.character(d$Referentie),
             dob = extract_dob(d),
             src = src,
             dnr = NA_character_)

  pbg <- new("individualBG",

             sex = extract_sex(b),

             # store GA in days and completed weeks
             gad = r$gad,
             ga  = trunc(r$gad / 7),

             # 1 = Nee, volgens BDS 1 = Ja, 2 = Nee
             smo = extract_field2(d, 91L, "ClientGegevens", "Elementen") - 1L,

             # in grammen, conform BSD
             bw = r$bw,

             # convert to cm
             hgtm = r$hgtm / 10,

             # convert to cm
             hgtf = r$hgtf / 10,

             # 510, passief roken, 1 = Nee, 2 = niet als..

             # agem (63 geboortedatum moeder, 62==2)
             agem = extract_agep(d, which_parent = "02"),

             # etn (71?)
             # etn = as.character(b[b$Bdsnummer == 71, 2])
             etn = "NL"

             # edu (66 opleiding moeder, 62==2)
  )

  if (length(d$Contactmomenten) == 0L) {
    time <- data.frame(age = numeric(), hgt = numeric(), wgt = numeric(),
                       hdc = numeric(), bmi = numeric())
  }
  else {
    time <-
      data.frame(
        age = as.numeric(round((r$dom - r$dob) / 365.25, 4L)),
        hgt = r$hgt / 10,
        wgt = r$wgt / 1000,
        hdc = r$hdc / 10,
        stringsAsFactors = FALSE)
    time$bmi <- time$wgt / (time$hgt / 100)^2

    # append birth weight record if needed
    if (!is.na(pbg@bw) && !any(is.na(time$age)) && !any(time$age == 0)) {
      time <- bind_rows(
        data.frame(age = 0,
                   wgt = pbg@bw / 1000),
        time)
      ds <- bind_rows(
        data.frame(age = 0),
        ds)
    }
  }

  if (is.null(time)) {
    pan <- new("individualAN")
    pbs <- new("individualBS")
    mil <- new("individualRW")
  } else {
    pan <- new("individualAN",
               hgt = new("xyz", yname = "hgt",
                         x = time$age,
                         y = as.numeric(time$hgt),
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga),
               wgt = new("xyz", yname = "wgt",
                         x = time$age,
                         y = as.numeric(time$wgt),
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga),
               hdc = new("xyz", yname = "hdc",
                         x = time$age,
                         y = as.numeric(time$hdc),
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga),
               bmi = new("xyz", yname = "bmi",
                         x = time$age,
                         y = as.numeric(time$bmi),
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga),
               wfh = new("xyz", yname = "wfh",
                         xname = "hgt",
                         x = time$hgt,
                         y = as.numeric(time$wgt),
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga),
               dsc = new("xyz", yname = "dsc",
                         x = ds$a,
                         y = ds$d,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga))
    pbs <- new("individualBS",
               bs.hgt = new("bse", yname = "hgt",
                            data = pan@hgt,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.wgt = new("bse", yname = "wgt",
                            data = pan@wgt,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.hdc = new("bse", yname = "hdc",
                            data = pan@hdc,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.bmi = new("bse", yname = "bmi",
                            data = pan@bmi,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.wfh = new("bse", yname = "wfh",
                            xname = "hgt",
                            data = pan@wfh,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.dsc = new("bse", yname = "dsc",
                            data = pan@dsc,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...))
    prw <- new("individualRW",
               ddi = new("ird", mst = time,
                         map = load_data(dnr = "smocc_bds"),
                         instrument = "ddi", ...))
  }

  new("individual", pid, pbg, pan, pbs, prw)
}

extract_dob <- function(d) {
  b <- d$ClientGegevens$Elementen
  dob <- ymd(b[b$Bdsnummer == 20, 2])
  if (length(dob) == 0L) return(as.Date(NA))
  dob
}

extract_sex <- function(b) {
  s <- b[b$Bdsnummer == 19L, 2L]
  if (length(s) == 0L) return(NA_character_)
  switch(s,
         "1" = "male",
         "2" = "female",
         NA_character_)
}

extract_agep <- function(d, which_parent = "02") {
  # returns age of parent in completed years
  # which_parent: "01" = father, "02" = mother
  dob <- extract_dob(d)
  p <- d$ClientGegevens$Groepen[[1]]
  if (is.null(p)) return(NA_real_)
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62, "Waarde"]
    if (parent == which_parent) {
      dobp <- ymd(pp[pp$Bdsnummer == 63, 2])
      agep <- as.numeric(trunc(difftime(dob, dobp, "days")/365.25))
      aa <- 1
      if (length(agep) == 0L) agep <- NA_real_
      return(agep)
    }
  }
  NA_real_
}

extract_field <- function(d, f = 245L) {
  z <- d$Contactmomenten[[2L]]
  as.numeric(unlist(lapply(z, function(x, f2 = f)
    ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA))))
}

extract_field2 <- function(d, f, l1, l2) {
  b <- d[[l1]][[l2]]
  v <- b[b$Bdsnummer == f, "Waarde"]
  ifelse (!length(v), NA_real_, as.numeric(v))
}

extract_field3 <- function(d, f, l1, l2, l3, which_parent = "02") {
  p <- d[[l1]][[l2]][[l3]]
  if (is.null(p)) return(NA)
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62L, "Waarde"]
    if (parent == which_parent) return(pp[pp$Bdsnummer == f, 2L])
  }
}
