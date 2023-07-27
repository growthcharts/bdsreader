convert_ddi_gsed_12 <- function(d, r, v) {

  # get BDS numbers and names of items
  bdsnum <- sort(unique(bdsreader::bds_gsed$bds))
  items <- bdsreader::bds_gsed$lex_gsed[bdsreader::bds_gsed$lex_gsed != ""]

  # premature return if there are no data
  if (!(length(d$Contactmomenten) || length(d$ContactMomenten) || length(d$clientMeasurements))) {
    return(data.frame(age = numeric(0)))
  }
  # premature return when no valid dates found
  if (is.null(r$dom)) {
    return(data.frame(age = numeric(0)))
  }

  # prepare the output matrices
  age <- as.numeric(round((r$dom - r$dob) / 365.25, 4L))
  if (length(age) == 0L) {return(data.frame(age = numeric(0)))}
  w <- data.frame(
    age = age,
    matrix(NA, nrow = length(age), ncol = length(bdsnum) + length(items))
  )
  names(w) <- c("age", bdsnum, items)

  # extract ddi data from bds-message
  # and convert to 0/1 scores
  for (i in bdsnum) {
    if (v == 1 | v == 2) {
      w[, as.character(i)] <- extract_field(d, i, v = v)
      next
    } else if (v == 3) {
      extr_bds <- extract_field(d, i, v = v)
      if (length(extr_bds) == 0L) {
        w[, as.character(i)] <- NA_real_
        next
      }
      w[, as.character(i)] <- extr_bds[match(r$dom, ymd(extr_bds$date)), "value"]
    }
  }

  for (item in items) {
    n <- which(bdsreader::bds_gsed$lex_gsed == item)
    type <- bdsreader::bds_gsed[n, "type"]
    bds <- bdsreader::bds_gsed[n, c("bds", "bdsr", "bdsl")]
    w[, item] <- switch(type,
                        g1 = recode(w[, as.character(bds[1L])], `1` = 1L, `2` = 0L, .default = NA_integer_),
                        m1 = recode(w[, as.character(bds[1L])], `1` = 1L, `2` = 0L, `3` = 1L, .default = NA_integer_),
                        g2 = {
                          bdsr <- w[, as.character(bds[2L])]
                          bdsl <- w[, as.character(bds[3L])]
                          pass <- as.numeric(bdsr == 1L & bdsl == 1L)
                          pass[is.na(bdsr) & is.na(bdsl)] <- NA
                          pass
                        },
                        m2 = {
                          bdsr <- w[, as.character(bds[2L])]
                          bdsl <- w[, as.character(bds[3L])]
                          pass <- as.numeric(bdsr %in% c(1L, 3L) & bdsl %in% c(1L, 3L))
                          pass[is.na(bdsr) & is.na(bdsl)] <- NA
                          pass
                        },
                        stop("Unrecognized type ", type, " for item ", item)
    )
  }

  # age-dependent post-processing to split items that map onto the same BDS number
  # we must remove scores that map onto the same BDS number at multiple ages (962, 986)

  # if age is missing, temporarily set it to -1 to evade error
  # "missing values are not allowed in subscripted assignments of data frames"
  age <- w$age
  age[is.na(age)] <- -1

  w[age < 0.5   / 365.25 | age > 42.5  / 365.25, "ddigmd155"] <- NA_real_
  w[age < 42.5  / 365.25 | age > 102.5 / 365.25, "ddigmd255"] <- NA_real_
  w[age < 102.5 / 365.25 | age > 146.5 / 365.25, "ddigmd355"] <- NA_real_
  w[age < 146.5 / 365.25 | age > 1 , "ddigmd055"] <- NA_real_

  w[age < 0.75 | age >= 1.75, "ddigmd068"] <- NA_real_
  w[age < 1.75 | age >= 2.50, "ddigmd168"] <- NA_real_
  w[age < 2.5  | age >= 3.50, "ddigmd268"] <- NA_real_

  # rename bds variables to bds_xxx
  names(w) <- c("age", paste0("bds", bdsnum), items)
  w
}

convert_ddi_gsed_3 <- function(d, r) {
  # premature return if there are no data
  if (!(length(d$Contactmomenten) ||
        length(d$ContactMomenten) ||
        length(d$clientMeasurements)) ||
      is.null(r$dom)) {
    return(list(bds = tibble(),
                items = tibble()))
  }

  # filter bds numbers of DDI, sort by bds and age
  bdsnum <- sort(unique(bdsreader::bds_gsed$bds))
  bds <- clientMeasurements_to_df(d$clientMeasurements) %>%
    filter(bds %in% bdsnum) %>%
    mutate(age = as.numeric(round((ymd(date) - r$dob) / 365.25, 4L))) %>%
    select(-date) %>%
    mutate(pass = recode(.data$value, `1` = 1L, `2` = 0L, `3` = 1L, .default = NA_integer_))
  # %>%
  #  pivot_wider(names_from = "age", values_from = c("pass"))

  # calculate pass/fail scores for DDI
  itm <- bdsreader::bds_gsed %>%
    filter(.data$lex_gsed != "") %>%
    select(c("lex_gsed", "type", "bds", "bdsr", "bdsl"))
  w1 <- left_join(bds, itm, by = "bds",
                  relationship = "many-to-many") %>%
    select(c("lex_gsed", "age", "pass")) %>%
    drop_na("lex_gsed")
  wr <- left_join(bds, itm, by = c("bds" = "bdsr"),
                  relationship = "many-to-one") %>%
    select(c("lex_gsed", "age", "pass"))
  wl <- left_join(bds, itm, by = c("bds" = "bdsl"),
                  relationship = "many-to-one") %>%
    select(c("lex_gsed", "age", "pass"))
  w2 <- left_join(wl, wr, by = c("lex_gsed", "age"),
                  relationship = "many-to-many") %>%
    mutate(pass = .data$pass.x * .data$pass.y) %>%
    select(c("lex_gsed", "age", "pass")) %>%
    drop_na("lex_gsed")
  w <- bind_rows(w1, w2)

  # apply post-hoc edits
  # age-dependent post-processing to split items that map onto the same BDS number
  # we must remove scores that map onto the same BDS number at multiple ages (962, 986)
  w[(w$age < 0.5   / 365.25 | w$age > 42.5  / 365.25) & w$lex_gsed == "ddigmd155", "pass"] <- NA_real_
  w[(w$age < 42.5  / 365.25 | w$age > 102.5 / 365.25) & w$lex_gsed == "ddigmd255", "pass"] <- NA_real_
  w[(w$age < 102.5 / 365.25 | w$age > 146.5 / 365.25) & w$lex_gsed == "ddigmd355", "pass"] <- NA_real_
  w[(w$age < 146.5 / 365.25 | w$age > 1) & w$lex_gsed == "ddigmd055", "pass"] <- NA_real_

  w[(w$age < 0.75 | w$age >= 1.75) & w$lex_gsed == "ddigmd068", "pass"] <- NA_real_
  w[(w$age < 1.75 | w$age >= 2.50) & w$lex_gsed == "ddigmd168", "pass"] <- NA_real_
  w[(w$age < 2.5  | w$age >= 3.50) & w$lex_gsed == "ddigmd268", "pass"] <- NA_real_

  list(bds = bds, items = w)
}
