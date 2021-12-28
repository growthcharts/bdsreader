names <- c("Laura S", "Thomas S", "Anne S")
ids <- as.integer(c(34071, 34072, 34073))

minidata <- donorloader::load_data(dnr = "smocc", ids = ids)
minidata$child$name <- names

minidata$child <- minidata$child[, c(1:13, 79)]
minidata$time  <- minidata$time[, c(1:78)]

usethis::use_data(minidata, overwrite = TRUE)
