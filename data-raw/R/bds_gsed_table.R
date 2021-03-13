# Create translation table BDS --> GSED

# append Van Wiechen BDS definition
project <- path.expand("~/Package/dscore/dscore")
fn <- file.path(project, "data-raw/data/bds_edited.csv")
ib <- read.csv2(file = fn, stringsAsFactors = FALSE)
idx <- !is.na(ib$bds) | !is.na(ib$bdsr)
bds_gsed <- ib[idx, c(
  "lex_gsed", "bds", "bdsr", "bdsl", "type",
  "bds_label", "labelEN"
)]
Encoding(bds_gsed$bds_label) <- "UTF-8"
Encoding(bds_gsed$labelEN) <- "UTF-8"

usethis::use_data(bds_gsed, overwrite = TRUE)
