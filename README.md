
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsreader

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `bdsreader` package is a lightweight package that

-   Reads and validates JSON files with data coded according to the
    Basisdataset JGZ protocol;
-   Produces S3 objects of class `target`.

The main use of the `bdsreader` is to translate incoming child data into
data objects for processing in `R`.

## Installation

Install the development version `bdsreader` by

``` r
install.packages("remotes")
remotes::install_github("growthcharts/bdsreader")
```

There is no release on CRAN.
