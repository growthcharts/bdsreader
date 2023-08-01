check_ranges_3 <- function(df) {

  adm <- attr(df, "adm")
  df <- df %>%
    mutate(type = ifelse(.data$type == "date", "tdate", .data$type)) %>%
    pivot_wider(values_from = "value", names_from = "type") %>%
    mutate(date = ifelse(is.na(.data$date), .data$tdate, .data$date)) %>%
    mutate(edit = "")
  if (hasName(df, "tdate")) {
    df <- select(df, -"tdate")
  }

  e <- catch_cnd({
    dates <- ymd(pull(df, "date"))
  })
  if (!is.null(e)) {
    message("Date conversion error.")
    if (hasName(df, "date")) {
      df <- repair(df, "date")
    }
  } else {
    df[["date"]] <- dates
  }

  e <- catch_cnd({
    categories <- as.integer(pull(df, "category"))
  })
  if (!is.null(e)) {
    message("Category conversion error.")
    if  (hasName(df, "category")) {
      df <- repair(df, "category")
    }
  } else {
    df[["category"]] <- categories
  }

  e <- catch_cnd({
    numbers <- as.numeric(pull(df, "number"))
  })
  if (!is.null(e)) {
    message("Number conversion error.")
    if  (hasName(df, "number")) {
      df <- repair(df, "number")
    }
  } else {
    df[["number"]] <- numbers
  }

  # check ranges
  ranges <- data.frame(
    bds = as.integer(c(82, 110, 238, 240, 235, 245, 252)),
    lo = c(50, 300, 800, 800, 100, 100, 100),
    hi = c(350, 8000, 3000, 3000, 3000, 300000, 900)
  )

  if (hasName(df, "number")) {
    df <- left_join(df, ranges, by = "bds") %>%
      mutate(oor = !is.na(.data$number) &
             (.data$number < .data$lo | .data$number > .data$hi),
             number = ifelse(.data$oor, NA_real_, .data$number),
             edit = ifelse(.data$oor, "* number", .data$edit)) %>%
      select(-c("lo", "hi", "oor"))
  }

  attr(df, "adm") <- adm
  return(df)
}

repair <- function(x, what) {
  tf <- switch(what,
               date = ymd,
               category = as.integer,
               number = as.numeric)
  ac <- switch(what,
               date = "* date",
               category = "* category",
               number = "* number")
  na <- switch(what,
               date = NA_Date_,
               category = NA_integer_,
               number = NA_real_)
  # convert one-by-one
  idx <- !is.na(x[[what]])
  broken <- x[[what]][idx]
  edit <- x[["edit"]][idx]
  repaired <- rep(na, length(broken))
  action <- rep("", length(broken))
  for (i in seq_along(broken)) {
    e <- catch_cnd({
      xt <- tf(broken[i])
    })
    if (!is.null(e)) {
      action[i] <- ac
    } else {
      repaired[i] <- xt
    }
  }

  m <- rep(na, nrow(x))
  m[idx] <- repaired
  x[[what]] <- m
  x[idx, "edit"] <- action
  return(x)
}

