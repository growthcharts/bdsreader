# update maria1.json and maria2.json
# careful: This script overwrites maria1.json and maria2.json
library(bdsreader)
library(tibble)

# donordata format
maria <- list(child = tibble(
  src = "1234",
  id = 5678,
  name = "Maria",
  dob = "11-10-18",
  sex = "female",
  gad = 189,
  smo = 1,
  bw = 990,
  dobf = "04-07-95",
  dobm = "02-12-90",
  hgtm = 167,
  hgtf = 190,
  etn = "NL"
), time = tibble(
  src = c("1234", "1234"),
  id = c(5678, 5678),
  age = c(0.0849, 0.167),
  sex = c("female", "female"),
  ga = c(27, 27),
  hgt = c(38, 43.5),
  wgt = c(1.250, 2.100),
  hdc = c(27, 30.5)
))
usethis::use_data(maria, overwrite = TRUE)

export_as_bds(maria, ids = 5678, indent = 2, organisation = 1234,
              names = "inst/examples/maria")
