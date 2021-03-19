convert_checked_list <- function(checked = NULL, append_ddi = FALSE) {
  d <- checked$data
  b <- d$ClientGegevens$Elementen
  r <- checked$ranges

  # convert ddi, calculate D-score
  ddi <- convert_ddi_gsed(d, r)
  ds <- dscore::dscore(data = ddi, key = "dutch")

  # store requester code
  src <- as.character(d$OrganisatieCode)
  src <- ifelse(length(src), src, "")
  persondata <- tibble(
    id = -1L,
    name = ifelse(length(d$Referentie), as.character(d$Referentie), NA_character_),
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
    xy <- tibble(
      age = numeric(),
      xname = character(), yname = character(),
      x = numeric(), y = numeric()
    )
  } else {
    xy <-
      tibble(
        age = rep(as.numeric(round((r$dom - r$dob) / 365.25, 4L)), 6L),
        xname = c(rep("age", length(r$dom) * 5L), rep("hgt", length(r$dom))),
        yname = rep(c("hgt", "wgt", "hdc", "bmi", "dsc", "wfh"), each = length(r$dom)),
        x = c(rep(as.numeric(round((r$dom - r$dob) / 365.25, 4L)), 5L), r$hgt / 10),
        y = c(
          r$hgt / 10,
          r$wgt / 1000,
          r$hdc / 10,
          (r$wgt / 1000) / (r$hgt / 1000)^2,
          ds$d,
          r$wgt / 1000
        )
      ) %>%
      tidyr::drop_na()
  }

  # append birth weight record if needed
  if (nrow(persondata) && !is.na(persondata$bw) && !any(is.na(xy$x)) && !any(xy$x == 0)) {
    xy <- bind_rows(
      xy,
      tibble(
        age = 0,
        xname = "age",
        yname = "wgt",
        x = 0,
        y = persondata$bw / 1000
      )
    )
  }

  ## append DDI
  if (nrow(ddi) && append_ddi) {
    xy <- bind_rows(
      xy,
      ddi %>%
        pivot_longer(
          cols = -all_of("age"), names_to = "yname",
          values_to = "y", values_drop_na = TRUE
        ) %>%
        mutate(
          xname = "age",
          x = .data$age
        )
    )
  }

  list(persondata = persondata, xy = xy)
}
