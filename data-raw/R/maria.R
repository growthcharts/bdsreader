# update maria1.json and maria2.json
# careful: This script overwrites maria1.json and maria2.json

fn <- system.file("examples", "maria2.json", package = "bdsreader")
xyz <- read_bds(fn, format = "2.0")

write_bds(xyz, format = "1.0", auto_format = FALSE, organisation = 1234L,
          file = "inst/examples/maria1.json", indent = 2)
write_bds(xyz, format = "2.0", organisation = 1234L,
          file = "inst/examples/maria2.json", indent = 2)
