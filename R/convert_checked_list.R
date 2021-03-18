convert_checked_list <- function(checked = NULL, ...) {
  d <- checked$data
  b <- d$ClientGegevens$Elementen
  r <- checked$ranges

  # convert ddi, calculate D-score
  ddi <- convert_ddi_gsed(d, r)
  ds <- dscore::dscore(data = ddi, key = "dutch")

  # browser()
  # is this child or message number?
  src <- as.character(d$OrganisatieCode)
  src <- ifelse(length(src), src, "")
  child <- tibble(
    id = 0L,
    name = as.character(d$Referentie),
    dob = extract_dob(d),
    src = src,
    dnr = NA_character_,
    sex = extract_sex(b),

    # store GA in days and completed weeks
    gad = r$gad,
    ga = trunc(r$gad / 7),

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

  if (!length(d$Contactmomenten)) {
    time <- tibble(
      age = numeric(), hgt = numeric(), wgt = numeric(),
      hdc = numeric(), bmi = numeric(), dsc = numeric()
    )
  } else {
    time <-
      tibble(
        age = as.numeric(round((r$dom - r$dob) / 365.25, 4L)),
        hgt = r$hgt / 10,
        wgt = r$wgt / 1000,
        hdc = r$hdc / 10,
        dsc = ds$d
      )
  }

  # append birth weight record if needed
  if (nrow(child) && !is.na(child$bw) && !any(is.na(time$age)) && !any(time$age == 0)) {
    time <- bind_rows(
      tibble(
        age = 0,
        wgt = child$bw / 1000
      ),
      time
    )
  }

  list(child = child, time = time, ddi = ddi)
}
