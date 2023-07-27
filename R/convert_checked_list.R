convert_checked_list <- function(d, r, append_ddi = FALSE, format = "1.0",
                                 ds = ds) {
  v <- as.integer(substr(format, 1L, 1L))
  b <- switch(v,
              d$ClientGegevens$Elementen,
              d$ClientGegevens,
              d$clientDetails)

  # store organisation code
  # support version- and case-insensitive reads
  src <- ""
  if (!is.null(d$organisatieCode)) src <- as.character(d$organisatieCode)
  if (!is.null(d$OrganisatieCode)) src <- as.character(d$OrganisatieCode)
  if (!is.null(d$organisationCode)) src <- as.character(d$organisationCode)
  if (!is.null(d$OrganisationCode)) src <- as.character(d$OrganisationCode)

  # store request name
  # support reference (recommended), Reference (legacy) and Referentie (legacy)
  name <- NA_character_
  if (!is.null(d$reference)) name <- as.character(d$reference)
  if (!is.null(d$Reference)) name <- as.character(d$Reference)
  if (!is.null(d$Referentie)) name <- as.character(d$Referentie)

  persondata <- tibble_row(
    id = -1L,
    name = name,
    dob  = extract_dob(d, which = "00", v = v),
    dobf = extract_dob(d, which = "01", v = v),
    dobm = extract_dob(d, which = "02", v = v),
    src = src,
    dnr = NA_character_,
    sex = extract_sex(b, v = v),

    # store GA in days and completed weeks
    gad = r$gad,
    ga = trunc(r$gad / 7),

    # Volgens BDS 1 = Ja, 2 = Nee
    smo = recode(extract_field2(d, 91L, v = v), `1` = 1L, `2` = 0L, .default = NA_integer_),

    # in grammen, conform BSD
    bw = r$bw,

    # convert to cm
    hgtm = r$hgtm / 10,

    # convert to cm
    hgtf = r$hgtf / 10,

    # 510, passief roken, 1 = Nee, 2 = niet als..

    # agem (63 geboortedatum moeder, 62==2)
    agem = extract_agep(.data$dob, .data$dobm),

    # etn (71?)
    # etn = as.character(b[b$Bdsnummer == 71, 2])
    etn = "NL"

    # edu (66 opleiding moeder, 62==2)
  )

  if (!length(d$Contactmomenten) && !length(d$ContactMomenten) && !length(d$clientMeasurements)) {
    xy <- tibble(
      age = numeric(),
      xname = character(), yname = character(),
      x = numeric(), y = numeric()
    )
  } else if (v == 1 | v == 2) {
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
  } else if (v == 3) {
    # v3 may have mismatched order of dates, which requires matching with pivot
    xy <- rbind(r$hgt, r$wgt, r$hdc)
    # if XY is empty
    if (length(xy) == 0L) xy <- data.frame(date = character(), value = numeric())
    # if one of the two variables is missing
    if (is.null(xy$date)) xy$date <- NA_Date_
    if (is.null(xy$value)) xy$value <- NA_real_
    xy <- xy %>%
      mutate(yname = rep(c("hgt", "wgt", "hdc"), times = c(nrow(r$hgt), nrow(r$wgt), nrow(r$hdc)))) %>%
      mutate(age = as.numeric(round((ymd(.data$date) - r$dob) / 365.25, 4L))) %>%
      pivot_wider(names_from = "yname", values_from = "value", values_fn = function(x) na.omit(x)[1])

    # add columns if they are missing
    cols <- c(hgt = NA_real_, wgt = NA_real_, hdc = NA_real_)
    xy <- add_column(xy, !!!cols[setdiff(names(cols), names(xy))]) %>%
      mutate(bmi = (.data$wgt / 1000) / (.data$hgt / 1000)^2) %>%
      mutate(hgt = .data$hgt / 10,
             wgt = .data$wgt / 1000,
             hdc = .data$hdc / 10) %>%
      # add d-score
      full_join(., ds %>% select(age = "a", dsc = "d"), by = "age") %>%
      select(-"date")

    if (nrow(xy) != 0L) {
      xy <- rbind(
        xy %>%
          pivot_longer(!"age", names_to = "yname", values_to = "y") %>%
          mutate(xname = "age", x = .data$age) %>%
          select("age", "xname", "yname", "x", "y"),
        xy %>%
          select("age", x = "hgt", "wgt") %>%
          pivot_longer("wgt", names_to = "yname", values_to = "y") %>%
          mutate(xname = "hgt", yname = "wfh")
      ) %>%
        tidyr::drop_na()
    }
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

  list(persondata = persondata, xy = xy)
}
