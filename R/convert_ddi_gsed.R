
convert_ddi_gsed <- function(d, r) {

  # get BDS numbers and names of items
  bds <- sort(unique(bdsreader::bds_gsed$bds))
  items <- bdsreader::bds_gsed$lex_gsed[bdsreader::bds_gsed$lex_gsed != ""]

  # premature return if there are no data
  if (length(d$Contactmomenten) == 0L)
    return(data.frame(age = numeric(0)))

  # prepare the output matrices
  age <- as.numeric(round((r$dom - r$dob) / 365.25, 4L))
  w <- data.frame(
    age = age,
    matrix(NA, nrow = length(age), ncol = length(bds) + length(items)))
  names(w) <- c("age", bds, items)

  # extract ddi data from bds-message
  # and convert to 0/1 scores
  for (i in bds) w[, as.character(i)] <- extract_field(d, i)
  for (item in items) {
    n <- which(bdsreader::bds_gsed$lex_gsed == item)
    type <- bdsreader::bds_gsed[n, "type"]
    bds  <- bdsreader::bds_gsed[n, c("bds", "bdsr", "bdsl")]
    w[, item] <- switch(type,
                        g1 = recode(w[, as.character(bds[1L])], `1` = 1L, `2` = 0L, .default = NA_integer_),
                        m1 = recode(w[, as.character(bds[1L])], `1` = 1L, `2` = 0L, `3` = 1L, .default = NA_integer_),
                        g2 = {
                          bdsr <- w[, as.character(bds[2L])]
                          bdsl <- w[, as.character(bds[3L])]
                          pass <- as.numeric(bdsr == 1L & bdsl == 1L)
                          pass[is.na(bdsr) & is.na(bdsl)] <- NA
                          pass},
                        m2 = {
                          bdsr <- w[, as.character(bds[2L])]
                          bdsl <- w[, as.character(bds[3L])]
                          pass <- as.numeric(bdsr %in% c(1L, 3L) & bdsl %in% c(1L, 3L))
                          pass[is.na(bdsr) & is.na(bdsl)] <- NA
                          pass},
                        stop("Unrecognized type ", type, " for item ", item)
    )
  }
  w[, c("age", items)]
}
