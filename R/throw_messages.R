throw_messages <- function(x) {
  if (length(x) == 0L) {
    return()
  }

  if (is.vector(x)) {
    for (i in seq_along(x)) {
      message(x[i])
    }
  }
  if (is.data.frame(x)) {
    for (i in seq_len(nrow(x))) {
      st <- paste0(
        "BDS ", format(x[i, 1L], width = 3, justify = "right"), " (",
        x[i, 2L], "): Supplied: ",
        x[i, 4L], ", Supplied type: ",
        x[i, 5L]
      )
      message(st)
    }
  }
}
