# Create the BDS lexicon

create_bds_lexicon <- function() {
  bds_lexicon <- data.frame(
    bdsnummer = c(
      19, 20, 62, 63, 66, 71, 82, 91, 110, 235, 245, 252,
      238, 240, 510
    ),
    description = c(
      "Geslacht",
      "Geboortedatum",
      "Relatie tot jeugdige ouder/verzorger",
      "Geboortedatum ouder/verzorger",
      "Opleiding ouder/verzorger",
      "Geboorteland ouder/verzorger",
      "Zwangerschapsduur",
      "Roken tijdens de zwangerschap",
      "Geboortegewicht",
      "Lengte",
      "Gewicht",
      "Hoofdomtrek",
      "Lengte biologische moeder",
      "Lengte biologische vader",
      "Passief roken in huis"
    ),
    description_EN = c(
      "Sex of child",
      "Date of birth",
      "Caretaker relation",
      "Caretaker date of birth",
      "Caretaker education",
      "Caretaker birth country",
      "Gestational age",
      "Smoking during pregnancy",
      "Birth weight",
      "Length/height",
      "Body weight",
      "Head circumference",
      "Height biological mother",
      "Height biological father",
      "Passive smoking"
    ),
    expected = c(
      "one of: 0, 1, 2, 3",
      "yyyymmdd",
      "one of: 01, 02, 03, 04, 05, 06, 07, 08, 98",
      "yyyymmdd",
      "one of: 01, 02, 03, 04, 05, 06, 07, 08, 98, 00",
      "4-digit code",
      "in days",
      "one of: 1, 2, 99",
      "in grammes",
      "in milimeters",
      "in grammes",
      "in milimeters",
      "in milimeters",
      "in milimeters",
      "one of: 01, 02, 03, 04"
    ),
    stringsAsFactors = FALSE
  )
}

bds_lexicon <- create_bds_lexicon()

library(dscore)
library(dplyr)
library(openxlsx)

# append Van Wiechen BDS definition
project <- path.expand("~/Package/dscore/dscore")
fn <- file.path(project, "data-raw/data/bds_edited.csv")
ib <- read.csv2(file = fn, stringsAsFactors = FALSE)
idx <- !is.na(ib$bds)
expected <- rep("one of: 1, 2", sum(idx))
expected[substr(ib$type[idx], 1, 1) == "m"] <- "one of: 1, 2, 3"
ddi <- data.frame(
  bdsnummer = ib$bds[idx],
  description = ib$bds_label[idx],
  description_EN = ib$labelEN[idx],
  expected = expected,
  stringsAsFactors = FALSE
)
Encoding(ddi$description) <- "UTF-8"
Encoding(ddi$description_EN) <- "UTF-8"

bds_lexicon <- rbind(bds_lexicon, ddi)
usethis::use_data(bds_lexicon, overwrite = TRUE)
