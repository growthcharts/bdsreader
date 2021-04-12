check_ranges <- function(d) {
  lex <- bdsreader::bds_lexicon

  e <- catch_cnd(dob <- ymd(extract_field2(d, 20L, "ClientGegevens", "Elementen")))
  if (length(dob) == 0L) {
    message("BDS  20 (",
      lex[lex$bdsnummer == 20, "description"],
      ") Missing",
      appendLF = FALSE
    )
  }
  if (!is.null(e)) {
    message("BDS  20 (",
      lex[lex$bdsnummer == 20, "description"],
      ") Onjuist format",
      appendLF = FALSE
    )
  }

  e <- catch_cnd(dobm <- ymd(extract_field3(d, 63L, "ClientGegevens", "Groepen", "Elementen")))
  if (!is.null(e)) {
    message("BDS  63 (",
      lex[lex$bdsnummer == 63, "description"],
      ") Onjuist format",
      appendLF = FALSE
    )
  }

  gad <- as.numeric(extract_field2(d, 82L, "ClientGegevens", "Elementen"))
  if (is.na(gad)) {
    message("BDS 82 (",
      lex[lex$bdsnummer == 82, "description"],
      " in dagen) heeft geen waarde",
      appendLF = FALSE
    )
  }
  if (!is.na(gad) & (gad < 50 | gad > 350)) {
    message("BDS 82 (",
      lex[lex$bdsnummer == 82, "description"],
      " in dagen): Buiten bereik 50-350",
      appendLF = FALSE
    )
    gad <- NA_real_
  }
  bw <- as.numeric(extract_field2(d, 110L, "ClientGegevens", "Elementen"))
  if (is.na(bw)) {
    message("BDS 110 (",
      lex[lex$bdsnummer == 110, "description"],
      " in grammen: heeft geen waarde",
      appendLF = FALSE
    )
  }
  if (!is.na(bw) & (bw < 300 | bw > 8000)) {
    message("BDS 110 (",
      lex[lex$bdsnummer == 110, "description"],
      " in grammen: Buiten bereik 300-8000",
      appendLF = FALSE
    )
  }

  hgtm <- as.numeric(extract_field2(d, 238L, "ClientGegevens", "Elementen"))
  if (is.na(hgtm)) {
    message("BDS 238 (",
      lex[lex$bdsnummer == 238, "description"],
      " in mm): heeft geen waarde",
      appendLF = FALSE
    )
  }
  if (!is.na(hgtm) & (hgtm < 800 | hgtm > 3000)) {
    message("BDS 238 (",
      lex[lex$bdsnummer == 238, "description"],
      " in mm): Buiten bereik 800-3000",
      appendLF = FALSE
    )
  }

  hgtf <- as.numeric(extract_field2(d, 240L, "ClientGegevens", "Elementen"))
  if (is.na(hgtm)) {
    message("BDS 240 (",
      lex[lex$bdsnummer == 240, "description"],
      " in mm): heeft geen waarde",
      appendLF = FALSE
    )
  }
  if (!is.na(hgtf) & (hgtf < 800 | hgtf > 3000)) {
    message("BDS 240 (",
      lex[lex$bdsnummer == 240, "description"],
      " in mm): Buiten bereik 800-3000",
      appendLF = FALSE
    )
  }

  hdc <- wgt <- hgt <- dom <- NULL

  if (length(d$Contactmomenten) == 0L) {
    message("Missing 'Contactmomenten'", appendLF = FALSE)
  } else {
    e <- catch_cnd(dom <- ymd(d$Contactmomenten[[1L]]))
    if (!is.null(e)) warning("Meetdatum: Onjuist format: ", as.character(d$Contactmomenten[[1L]]))

    hgt <- extract_field(d, 235L)
    wgt <- extract_field(d, 245L)
    hdc <- extract_field(d, 252L)

    if (all(is.na(hgt))) {
      message("BDS 235 (",
        lex[lex$bdsnummer == 235, "description"],
        " in mm): heeft geen waarde",
        appendLF = FALSE
      )
    }
    if (any(!is.na(hgt) & (hgt < 100 | hgt > 3000))) {
      message("BDS 235 (",
        lex[lex$bdsnummer == 235, "description"],
        " in mm): Buiten bereik 100-2500",
        appendLF = FALSE
      )
    }
    if (all(is.na(wgt))) {
      message("BDS 245 (",
        lex[lex$bdsnummer == 245, "description"],
        " in grammen): heeft geen waarde",
        appendLF = FALSE
      )
    }
    if (any(!is.na(wgt) & (wgt < 100 | wgt > 300000))) {
      message("BDS 245 (",
        lex[lex$bdsnummer == 245, "description"],
        " in grammen): Buiten bereik 100-300000",
        appendLF = FALSE
      )
    }
    if (all(is.na(hdc))) {
      message("BDS 252 (",
        lex[lex$bdsnummer == 252, "description"],
        " in mm): heeft geen waarde",
        appendLF = FALSE
      )
    }
    if (any(!is.na(hdc) & (hdc < 100 | hdc > 900))) {
      message("BDS 252 (",
        lex[lex$bdsnummer == 252, "description"],
        " in mm): Buiten bereik 100-900",
        appendLF = FALSE
      )
    }
  }

  if (length(d$ClientGegevens$Groepen) == 0L) {
    message("Missing 'ClientGegevens$Groepen'", appendLF = FALSE)
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
