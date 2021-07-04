# reprex

library(bdsreader)
jtf <- system.file("extdata", paste0("bds", pad), "test", paste0("test", 1:25, ".json"), package = "jamesdemodata")
jtf[1]
read_bds(jtf[1], version = 2)
