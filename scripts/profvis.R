library(profvis)
library(bdsreader)
profvis({
  fn <- system.file("extdata/bds_v3.0/smocc/Laura_S.json", package = "jamesdemodata")
  # fn <- system.file("examples/maria.json", package = "bdsreader")
  m <- read_bds(fn)
})
