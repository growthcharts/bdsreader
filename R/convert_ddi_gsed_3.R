convert_ddi_gsed_3 <- function(bds) {
  dob <- filter(bds, bds == 20L) %>% pull(.data$date) %>% first()
  bdsnum <- sort(unique(bdsreader::bds_gsed$bds))
  ddi <- bds %>%
    filter(.data$bds %in% !! bdsnum) %>%
    mutate(age = as.numeric(round((.data$date - !! dob) / 365.25, 4L))) %>%
    mutate(pass = recode(.data$category, `1` = 1L, `2` = 0L, `3` = 1L,
                         .default = NA_integer_)) %>%
    select(c("bds", "age", "pass"))

  # calculate pass/fail scores for DDI
  itm <- bdsreader::bds_gsed %>%
    filter(.data$lex_gsed != "") %>%
    select(c("lex_gsed", "type", "bds", "bdsr", "bdsl"))
  w1 <- left_join(ddi, itm, by = "bds",
                  relationship = "many-to-many") %>%
    select(c("lex_gsed", "age", "pass")) %>%
    drop_na("lex_gsed")
  wr <- left_join(ddi, itm, by = c("bds" = "bdsr"),
                  relationship = "many-to-one") %>%
    select(c("lex_gsed", "age", "pass"))
  wl <- left_join(ddi, itm, by = c("bds" = "bdsl"),
                  relationship = "many-to-one") %>%
    select(c("lex_gsed", "age", "pass"))
  w2 <- left_join(wl, wr, by = c("lex_gsed", "age"),
                  relationship = "many-to-many") %>%
    mutate(pass = .data$pass.x * .data$pass.y) %>%
    select(c("lex_gsed", "age", "pass")) %>%
    drop_na("lex_gsed")
  w <- bind_rows(w1, w2)

  # Age-dependent post-processing to split items that map onto the
  # same BDS number. We must remove scores that map onto the same BDS
  # number at multiple ages (962, 986).
  w[(w$age < 0.5   / 365.25 | w$age > 42.5  / 365.25) &
      w$lex_gsed == "ddigmd155", "pass"] <- NA_real_
  w[(w$age < 42.5  / 365.25 | w$age > 102.5 / 365.25) &
      w$lex_gsed == "ddigmd255", "pass"] <- NA_real_
  w[(w$age < 102.5 / 365.25 | w$age > 146.5 / 365.25) &
      w$lex_gsed == "ddigmd355", "pass"] <- NA_real_
  w[(w$age < 146.5 / 365.25 | w$age > 1) &
      w$lex_gsed == "ddigmd055", "pass"] <- NA_real_

  w[(w$age < 0.75 | w$age >= 1.75) &
      w$lex_gsed == "ddigmd068", "pass"] <- NA_real_
  w[(w$age < 1.75 | w$age >= 2.50) &
      w$lex_gsed == "ddigmd168", "pass"] <- NA_real_
  w[(w$age < 2.5  | w$age >= 3.50) &
      w$lex_gsed == "ddigmd268", "pass"] <- NA_real_

  return(w)
}
