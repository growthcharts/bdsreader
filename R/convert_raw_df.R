convert_raw_df <- function(d) {
  df <- bind_rows(
    convert_cli_df(d[["clientDetails"]]),
    convert_msr_df(d[["clientMeasurements"]]),
    convert_nested_df(d[["nestedDetails"]])) %>%
    filter(!.data$type == "NA") %>%
    select(all_of(c("bds", "nest", "code", "date", "type", "value")))
  attr(df, "adm") <-
    list(
      fmt = d$Format,
      org = d$organisationCode,
      ref = d$reference)

  # if there is a birth weight, add dob as its date
  dob <- filter(df, .data$bds == 20L) %>%
    pull("value") %>%
    first()
  df[df$bds == 110L, "date"] <- dob

  return(df)
}

convert_cli_df <- function(x) {
  # convert fixed child-level covariates
  if (is.null(x[["bdsNumber"]])) {
    return(tibble(
      bds = integer(0),
      date = character(0),
      type = character(0),
      value = character(0))
    )
  } else {
    return(tibble(
      bds = x[["bdsNumber"]],
      date = NA_character_,
      type = set_type(x[["bdsNumber"]]),
      value = as.character(x[["value"]])
    ))
  }
}

convert_msr_df <- function(x) {
  # convert time-varying measurements
  xv <- x[["values"]]
  for (i in seq_along(xv)) {
    xv[[i]][["value"]] <- as.character(xv[[i]][["value"]])
  }
  bds <- rep(x[["bdsNumber"]], sapply(xv, nrow))
  x <- bind_cols(bds = bds,
            type = set_type(bds),
            bind_rows(xv))
  if (all(hasName(x, c("bds", "date")))) {
    x <- x %>%
      distinct(.data$bds, .data$date, .keep_all = TRUE)
  }
  return(x)
}

convert_nested_df <- function(x) {
  # support one nesting level, parent/sibling measurements
  nest <- x[["nestingBdsNumber"]]
  if (is.null(nest)) {
    return(tibble(nest = integer(0),
                  code = character(0))
    )
  }

  code <- x[["nestingCode"]]
  cli <- x[["clientDetails"]]
  msr <- x[["clientMeasurements"]]
  for (i in seq_along(nest)) {
    cli[[i]] <- convert_cli_df(cli[[i]])
    msr[[i]] <- convert_msr_df(msr[[i]])
  }

  rows <- c(sapply(cli, nrow), sapply(msr, nrow))
  nest <- rep(rep(nest, 2L), rows)
  code <- rep(rep(code, 2L), rows)

  bind_cols(
    bind_rows(cli, msr),
    nest = nest,
    code = code)
}

set_type <- function(bds) {
  category <- c(19, 62, 66, 91, 510, 879, 881, 883, 884, 885,
                887, 888, 889, 890, 891, 894, 896, 897, 898,
                902, 903, 906, 907, 910, 912, 914, 916, 917,
                918, 920, 922, 923, 926, 945, 951, 955, 956,
                958, 959, 961, 962, 964, 966, 968, 970, 971,
                973, 975, 977, 978, 986, 989, 991, 993, 994,
                996, 999, 1002, 886, 892, 893, 900, 905, 909,
                913, 921, 927, 928, 930, 931, 932, 933, 934,
                935, 936, 937, 938, 939, 940, 941, 943, 947,
                948, 949, 950, 953, 954, 972, 980, 982, 984,
                998, 1001, 1278)
  date <- c(20, 63)
  integer <- c(71)
  number <- c(82, 110, 235, 238, 240, 245, 252)
  character <- c(16)

  type <- rep(NA_character_, length(bds))
  type[bds %in% category] <- "category"
  type[bds %in% date] <- "date"
  type[bds %in% integer] <- "integer"
  type[bds %in% number] <- "number"
  type[bds %in% character] <- "character"
  return(type)
}
