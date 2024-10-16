check_ranges_3 <- function(df) {

  adm <- attr(df, "adm")
  df <- df %>%
    mutate(type = ifelse(.data$type == "date", "tdate", .data$type)) %>%
    # use only the first record for the parent code cf #10
    group_by(.data$bds, .data$nest, .data$code) %>%
    slice_head(n = 1L) %>%
    ungroup() %>%
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
    if (hasName(df, "date")) {
      message("Date conversion error.")
      df <- repair(df, "date")
    }
  } else {
    df[["date"]] <- dates
  }

  e <- catch_cnd({
    categories <- as.integer(pull(df, "category"))
  })
  if (!is.null(e)) {
    if  (hasName(df, "category")) {
      message("Category conversion error.")
      df <- repair(df, "category")
    }
  } else {
    df[["category"]] <- categories
  }

  e <- catch_cnd({
    numbers <- as.numeric(pull(df, "number"))
  })
  if (!is.null(e)) {
    if  (hasName(df, "number")) {
      message("Number conversion error.")
      df <- repair(df, "number")
    }
  } else {
    df[["number"]] <- numbers
  }

  # e <- catch_cnd({
  #   integers <- as.integer(pull(df, "integer"))
  # })
  # if (!is.null(e)) {
  #   if  (hasName(df, "integer")) {
  #     message("Integer conversion error.")
  #     df <- repair(df, "integer")
  #   }
  # } else {
  #   df[["integer"]] <- integers
  # }

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
               number = as.numeric,
               integer = as.integer)
  ac <- switch(what,
               date = "* date",
               category = "* category",
               number = "* number",
               integer = "* integer")
  na <- switch(what,
               date = NA_Date_,
               category = NA_integer_,
               number = NA_real_,
               integer = NA_integer_)
  # convert one-by-one
  idx <- !is.na(x[[what]])
  broken <- x[[what]][idx]
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
