sideload_variables <- function(d) {
  variables <- list()
  psn <- d[["clientDetails"]]
  xy <- d[["clientMeasurements"]]

  # convert fixed child-level covariates
  if (is.null(psn[["varName"]])) {
    variables[["psn"]] <- tibble(
      varName = integer(0),
      value = character(0)
    )
  } else {
    variables[["psn"]] <- tibble(
      varName = psn[["varName"]],
      value = as.character(psn[["value"]])
    ) %>%
      filter(!is.na(.data[["varName"]]))
  }

  # convert time-level covariates
  if (is.null(xy[["varName"]])) {
    variables[["xy"]] <- tibble(
      date = integer(0),
      varName = integer(0),
      value = character(0)
    )
  } else {
    variables[["xy"]] <- xy[!is.na(xy$varName), ] %>%
      unnest(cols = "values") %>%
      mutate(date = ymd(date)) %>%
      select(all_of(c("date", "varName", "value")))
  }

  return(variables)
}

# BDS allows a 6th pubic-hair stage (P6) for phb/phg (bdsNumber 315/825, or
# their varName sideload equivalents); tanner's reference tables only
# tabulate stages 1-5, so P6 is folded into P5 before it reaches
# tanner::calculate_sds().
recode_pubertal_p6 <- function(xy) {
  xy %>%
    mutate(
      y = ifelse(.data$yname %in% c("phb", "phg"), pmin(.data$y, 5), .data$y)
    )
}
