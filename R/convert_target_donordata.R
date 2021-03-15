#' Convert target object to donordata
#'
#' This function transforms an object of class `target` into a
#' donordata object, a list with elements named `"child"`
#' and `"time"`.
#' @param x An object of class `target`.
#' @return A list of two tibbles
#' @author Stef van Buuren 2021
#' @examples
#' fn <- system.file("extdata", "allegrosultum", "client3.json", package = "jamestest")
#' tgt <- convert_bds_target(fn)
#' target_to_donordata(tgt)
#' @export
target_to_donordata <- function(x) {
  if (!is.target(x)) stop("Object not of class 'target'.")

  data <- x$time %>%
     mutate(sex = (!!x)$child$sex,
            ga = (!!x)$child$ga,
            bmi = .data$wgt / (.data$hgt / 100)^2)
  data <- bind_cols(
    select(data, c(-c("sex", "ga"))),
           transform2z(data))

  list(child = x$child, time = data)
}
