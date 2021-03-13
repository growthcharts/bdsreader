## Official Country Table used by Dutch Government
fn <- file.path(getwd(), "data-raw", "data", "Tabel34_Landentabel_ASCII.txt")
table34 <- read.table(fn,
  stringsAsFactors = FALSE, header = TRUE,
  sep = "\t", fileEncoding = "macintosh"
)[, 1:2]
names(table34) <- c("code", "country")
usethis::use_data(table34, overwrite = TRUE)

# x <- "Abessini\xEB"
# Encoding(x) <- "latin1"
# x
# xx <- iconv(x, "latin1", "UTF-8")
# xx
