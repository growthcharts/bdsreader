check_ranges <- function(d, format) {
  v <- as.integer(substr(format, 1L, 1L))
  lex <- bdsreader::bds_lexicon

  e <- catch_cnd(dob <- ymd(extract_field2(d, 20L, v = v)))
  if (!is.null(e)) {
    message("BDS  20 (",
            lex[lex$bdsnummer == 20, "description"],
            ") Onjuiste geboortedatum"
    )
    dob <- NA
  } else if (length(dob) == 0L) {
    message("BDS  20 (",
            lex[lex$bdsnummer == 20, "description"],
            ") Onjuiste geboortedatum"
    )
    dob <- NA
  }

  e <- catch_cnd(dobm <- ymd(extract_field3(d, 63L, v = v)))
  if (!is.null(e)) {
    message("BDS  63 (",
            lex[lex$bdsnummer == 63, "description"],
            ") Onjuist format"
    )
    dobm <- NA
  }

  gad <- as.numeric(extract_field2(d, 82L, v = v))
  if (is.na(gad)) {
    message("BDS 82 (",
            lex[lex$bdsnummer == 82, "description"],
            " in dagen) heeft geen waarde"
    )
  }
  if (!is.na(gad) & (gad < 50 | gad > 350)) {
    message("BDS 82 (",
            lex[lex$bdsnummer == 82, "description"],
            " in dagen): Buiten bereik 50-350"
    )
    gad <- NA_real_
  }
  bw <- as.numeric(extract_field2(d, 110L, v = v))
  if (is.na(bw)) {
    message("BDS 110 (",
            lex[lex$bdsnummer == 110, "description"],
            " in grammen: heeft geen waarde"
    )
  }
  if (!is.na(bw) & (bw < 300 | bw > 8000)) {
    message("BDS 110 (",
            lex[lex$bdsnummer == 110, "description"],
            " in grammen: Buiten bereik 300-8000"
    )
  }

  hgtm <- as.numeric(extract_field2(d, 238L, v = v))
  if (is.na(hgtm)) {
    message("BDS 238 (",
            lex[lex$bdsnummer == 238, "description"],
            " in mm): heeft geen waarde"
    )
  }
  if (!is.na(hgtm) & (hgtm < 800 | hgtm > 3000)) {
    message("BDS 238 (",
            lex[lex$bdsnummer == 238, "description"],
            " in mm): Buiten bereik 800-3000"
    )
  }

  hgtf <- as.numeric(extract_field2(d, 240L, v = v))
  if (is.na(hgtm)) {
    message("BDS 240 (",
            lex[lex$bdsnummer == 240, "description"],
            " in mm): heeft geen waarde"
    )
  }
  if (!is.na(hgtf) & (hgtf < 800 | hgtf > 3000)) {
    message("BDS 240 (",
            lex[lex$bdsnummer == 240, "description"],
            " in mm): Buiten bereik 800-3000"
    )
  }

  hdc <- wgt <- hgt <- dom <- NULL

  if (length(d$Contactmomenten) == 0L &
      length(d$ContactMomenten) == 0L &
      length(d$clientMeasurements) == 0L) {
    switch(v,
           message("Missing 'Contactmomenten'"),
           message("Missing 'ContactMomenten'"),
           message("Missing 'clientMeasurements'"))
  } else {
    if (v == 1) {
      e <- catch_cnd(dom <- ymd(d$Contactmomenten[[1L]]))
      if (!is.null(e)) warning("Meetdatum: Onjuist format: ", as.character(d$Contactmomenten[[1L]]))
    } else if (v == 2) {
      e <- catch_cnd(dom <- ymd(d$ContactMomenten[[1L]]))
      if (!is.null(e)) warning("Meetdatum: Onjuist format: ", as.character(d$ContactMomenten[[1L]]))
    } else if (v == 3) {
      e <- catch_cnd(dom <- unique(ymd(do.call("rbind", d$clientMeasurements$values)$date)))
      if (!is.null(e)) warning("Meetdatum: Onjuist format: ", as.character(d$ContactMomenten[[1L]]))
    }

    hgt <- extract_field(d, 235L, v = v)
    wgt <- extract_field(d, 245L, v = v)
    hdc <- extract_field(d, 252L, v = v)

    if (all(is.na(switch(v, hgt, hgt, hgt$value)))) {
      message("BDS 235 (",
              lex[lex$bdsnummer == 235, "description"],
              " in mm): heeft geen waarde"
      )
    }
    if (any(!is.na(switch(v, hgt, hgt, hgt$value)) &
            (switch(v, hgt, hgt, hgt$value) < 100 |
             switch(v, hgt, hgt, hgt$value) > 3000))) {
      message("BDS 235 (",
              lex[lex$bdsnummer == 235, "description"],
              " in mm): Buiten bereik 100-2500"
      )
    }
    if (all(is.na(switch(v, wgt, wgt, wgt$value)))) {
      message("BDS 245 (",
              lex[lex$bdsnummer == 245, "description"],
              " in grammen): heeft geen waarde"
      )
    }
    if (any(!is.na(switch(v, wgt, wgt, wgt$value)) &
            (switch(v, wgt, wgt, wgt$value) < 100 |
             switch(v, wgt, wgt, wgt$value) > 300000))) {
      message("BDS 245 (",
              lex[lex$bdsnummer == 245, "description"],
              " in grammen): Buiten bereik 100-300000"
      )
    }
    if (all(is.na(switch(v, hdc, hdc, hdc$value)))) {
      message("BDS 252 (",
              lex[lex$bdsnummer == 252, "description"],
              " in mm): heeft geen waarde"
      )
    }
    if (any(!is.na(switch(v, hdc, hdc, hdc$value)) &
            (switch(v, hdc, hdc, hdc$value) < 100 |
             switch(v, hdc, hdc, hdc$value) > 900))) {
      message("BDS 252 (",
              lex[lex$bdsnummer == 252, "description"],
              " in mm): Buiten bereik 100-900"
      )
    }
  }

  if (v == 1) {
    if (length(d$ClientGegevens$Groepen) == 0L) {
      message("Missing 'ClientGegevens$Groepen'")
    }
  } else if (v == 2) {
    if (all(unlist(lapply(d$ClientGegevens$GenesteElementen, is.null)))) {
      message("Missing 'ClientGegevens$GenesteElementen'")
    }
  } else if (v == 3) {
    if (length(d$nestedDetails) == 0L) {
      message("Missing 'nestedDetails'")
    }
  }


  list(
    dob = dob,
    dobm = dobm,
    gad = gad,
    bw = bw,
    hgtm = hgtm,
    hgtf = hgtf,
    dom = dom,
    hgt = hgt,
    wgt = wgt,
    hdc = hdc
  )
}
