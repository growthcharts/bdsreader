# v1.0 keep for backward compatibility
# v2.0 new schema - June 2021 - compatibility with Eaglescience
schemas <- c(system.file("schemas/bds_v1.0.json", package = "bdsreader", mustWork = TRUE),
             system.file("schemas/bds_v2.0.json", package = "bdsreader", mustWork = TRUE))
paths <-   c("bds_v1.0",
             "bds_v2.0")

format <- 2
schema <- schemas[format]
path <- paths[format]

jtf <- system.file("extdata", path, "test",
                   paste0("test", 1:25, ".json"),
                   package = "jamesdemodata")

d <- jsonlite::fromJSON(jtf[11])
d[["ClientGegevens"]][["GenesteElementen"]][[8]]
tgt <- read_bds(jtf[11], format = 2)
js <- write_bds(tgt, format = format, org = 10, check = TRUE)
d2 <- jsonlite::fromJSON(js)
d2[["ClientGegevens"]][["GenesteElementen"]][[9]]
identical(d[["ClientGegevens"]][["GenesteElementen"]][[8]], d2[["ClientGegevens"]][["GenesteElementen"]][[8]])

# save to file
js <- prettify(js)
writeLines(text = js, con = "temp.json")

