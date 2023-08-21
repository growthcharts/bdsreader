convert_checked_list_3 <- function(bds, ds) {

  tgt <- make_target(NULL)
  psn <- persondata(tgt)

  # fixed child-level covariates
  psn[["id"]] <- -1L

  name <- as.character(attr(bds, "adm")[["ref"]])
  if (!length(name)) name <- NA_character_
  psn[["name"]] <- name

  src <- as.character(attr(bds, "adm")[["org"]])
  if (!length(src)) src <- NA_character_
  psn[["src"]] <- src

  if (hasName(bds, "date")) {
    psn[["dob"]] <- filter(bds, bds == 20L) %>%
      pull("date") %>%
      first()
  }

  # BDS variables, child level
  if (hasName(bds, "category")) {
    psn[["sex"]] <- filter(bds, bds == 19L) %>%
      pull("category") %>%
      first() %>%
      case_match(1 ~ "male", 2 ~ "female")
    psn[["smo"]] <- filter(bds, bds == 91L) %>%
      pull("category") %>%
      first() %>%
      case_match(1 ~ 1, 2 ~ 0)
  }

  if (hasName(bds, "number")) {
    psn[["gad"]] <- filter(bds, bds == 82L) %>%
      pull("number") %>%
      first() %>%
      as.numeric()
    psn[["ga"]] <- trunc(psn[["gad"]] / 7)
    psn[["bw"]] <- filter(bds, bds == 110L) %>%
      pull("number") %>%
      first() %>%
      as.numeric()
    psn[["hgtm"]] <- filter(bds, bds == 238L) %>%
      mutate(number = .data$number / 10) %>%
      pull("number") %>%
      first()
    psn[["hgtf"]] <- filter(bds, bds == 240L) %>%
      mutate(number = .data$number / 10) %>%
      pull("number") %>%
      first()
    psn[["agem"]] <- as.numeric(trunc((psn[["dob"]] - psn[["dobm"]]) / 365.25))
  }

  if (hasName(bds, "character")) {
    psn[["pc4"]] <- filter(bds, bds == 16L) %>%
      pull("character") %>%
      first()
    psn[["etn"]] <- "NL"
  }

  if (hasName(bds, "integer")) {
    psn[["par"]] <- filter(bds, bds == 471L) %>%
      pull("integer") %>%
      first() |>
      as.integer()
  }

  # Nested fields BDS 62
  if (all(hasName(bds, c("date", "nest", "code")))) {
    psn[["dobf"]] <-
      filter(bds, bds == 63L & .data$nest == 62L & .data$code == "01") %>%
      pull("date") %>%
      first()
    psn[["dobm"]] <-
      filter(bds, bds == 63L & .data$nest == 62L & .data$code == "02") %>%
      pull("date") %>%
      first()
  }

  if (all(hasName(bds, c("integer", "nest", "code")))) {
    psn[["blbf"]] <-
      filter(bds, bds == 71L & .data$nest == 62L & .data$code == "01") %>%
      pull("integer") %>%
      first() %>%
      as.integer()
    psn[["blbm"]] <-
      filter(bds, bds == 71L & .data$nest == 62L & .data$code == "02") %>%
      pull("integer") %>%
      first() %>%
      as.integer()
  }

  if (all(hasName(bds, c("category", "nest", "code")))) {
    psn[["eduf"]] <-
      filter(bds, bds == 66L & .data$nest == 62L & .data$code == "01") %>%
      pull("category") %>%
      first()
    psn[["edum"]] <-
      filter(bds, bds == 66L & .data$nest == 62L & .data$code == "02") %>%
      pull("category") %>%
      first()
  }

  # time-varying child data
  bdsnum <- as.integer(c(110, 235, 245, 252))
  f <- data.frame(
    bds = bdsnum,
    divider = c(1000, 10, 1000, 10),
    yname = c("wgt", "hgt", "wgt", "hdc")
  )
  xy <- tibble(
    age = numeric(0),
    xname = character(0),
    yname = character(0),
    x = numeric(0),
    y = numeric(0))

  if (all(hasName(bds, c("bds", "date", "number")))) {
    xy <- bds %>%
      filter(bds %in% bdsnum) %>%
      left_join(f, by = "bds") %>%
      mutate(
        age = as.numeric(round((.data$date - !!psn$dob) / 365.25, 4)),
        xname = "age",
        x = .data$age,
        y = .data$number / .data$divider) %>%
      select(c("age", "xname", "yname", "x", "y")) %>%
      arrange(factor(.data$yname, levels = c("hgt", "wgt", "hdc")),
              .data$age) %>%
      distinct(.data$age, .data$xname, .data$yname, .keep_all = TRUE)
  }

  # add wfh, bmi and dsc
  dsc <- wfh <- bmi <- tibble()
  if (all(hasName(xy, c("yname", "y")))) {
    wide <- xy %>%
      filter(.data$yname %in% c("hgt", "wgt")) %>%
      pivot_wider(names_from = "yname",
                  values_from = "y",
                  values_fn = function(x) na.omit(x)[1L])
  }
  if (all(hasName(wide, c("wgt", "hgt")))) {
    bmi <- wide %>%
      mutate(y = .data$wgt / (.data$hgt / 100)^2,
             yname = "bmi") %>%
      select(c("age", "xname", "yname", "x", "y"))
    wfh <- wide %>%
      select(-"x") %>%
      mutate(yname = "wfh",
             xname = "hgt") %>%
      rename(x = "hgt", y = "wgt") %>%
      select(c("age", "xname", "yname", "x", "y"))
  }
  if (all(hasName(ds, c("a", "d")))) {
    dsc <- ds %>%
      select(all_of(c("a", "d"))) %>%
      mutate(xname = "age",
             yname = "dsc",
             x = .data$a) %>%
      rename(age = "a", y = "d") %>%
      distinct(.data$age, .keep_all = TRUE) %>%
      select(all_of(c("age", "xname", "yname", "x", "y")))
  }

  if (all(hasName(xy, c("age", "yname")))) {
    xy <- xy %>%
      bind_rows(bmi, wfh, dsc) %>%
      arrange(factor(.data$yname,
                     levels = c("hgt", "wgt", "hdc", "bmi", "dsc", "wfh")),
              .data$age)
  }

  list(psn = psn, xy = xy)
}
