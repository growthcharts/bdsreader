# # Smit March 2022
# # Problem: Unexpected behaviour of growth predictor
#
# # age group: 4-18 years
# # matches: 4
# # low age: 3m
# # high age: 48m
#
# fn <- "tests/testthat/bug-pred-4.js"
#
# data <- read_bds(
#   txt = fn,
#   verbose = TRUE
# )
#
# library(chartplotter)
# library(grid)
# g <- process_chart(target = data, chartcode = "NJCH", dnr = "4-18", period = c(0.25, 4), nmatch = 5)
#
# # "problem plot"
# g <- process_chart(target = data, chartcode = "NJCH",
#                    dnr = "4-18", period = c(0.25, 4), nmatch = 5,
#                    show_future = TRUE, show_realized = TRUE,
#                    curve_interpolation = FALSE)
# grid.draw(g)
# # plot using curve interpolation
# g <- process_chart(target = data, chartcode = "NJCH",
#                    dnr = "4-18", period = c(0.25, 4), nmatch = 5,
#                    show_future = TRUE, show_realized = TRUE,
#                    curve_interpolation = TRUE)
# grid.draw(g)
#
# # rq2 error
# g <- process_chart(target = data, chartcode = "NJBH",
#                    dnr = "2-4", period = c(0, 3.75), nmatch = 5,
#                    show_future = TRUE, show_realized = TRUE,
#                    curve_interpolation = TRUE)
# grid.draw(g)
# # Diagnosis: Programming error. No data to match on.
# # We may reproduce the error with https://tnochildhealthstatistics.shinyapps.io/james_tryout/
# # choices: Groep=Graham, Naam kind=Sven G, Matches: 10 voorspelhorizon: 2-4,
# # bezoek: (4m-45m werkt),
# # bezoek: (3m-45) geeft Server error (rq2 - draw_chart): In call: select(., all_of(c(".row", !!vars)))
# # The problem also occurs in JAMES 1.2.0
