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
      unnest(cols = .data$values) %>%
      mutate(date = ymd(date)) %>%
      select(all_of(c("date", "varName", "value")))
  }

  return(variables)
}
