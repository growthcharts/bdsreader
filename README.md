
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsreader

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/growthcharts/bdsreader/workflows/R-CMD-check/badge.svg)](https://github.com/growthcharts/bdsreader/actions)
<!-- badges: end -->

The `bdsreader` package is a lightweight package that

-   Reads and parses child data coded according to the Basisdataset JGZ
    protocol;
-   Compares the child data against one of two JSON validation schema;
-   Calculates the child’s D-score from the Van Wiechen Schema (DDI)
    responses;
-   Adds Z-scores for height, weight, head circumference, BMI,
    weight-for-height and D-score;
-   Converts the result into a a list with person-level and time-level
    data.

The `bdsreader` translates child data (incoming via an API request) into
a data object useful for `R` processing. The package is part of Joint
Automatic Measurement and Evaluation System (JAMES) developed by the
Netherlands Organisation for Applied Scientific Research TNO.

## Installation

Install the development version `bdsreader` by

``` r
install.packages("remotes")
remotes::install_github("growthcharts/bdsreader")
```

There is no CRAN release.

## Example

The following commands illustrate the main use of `bdsreader`.

``` r
library(bdsreader)
fn <- system.file("examples", "maria2.json", package = "bdsreader")
tgt <- read_bds(fn)
timedata(tgt)
#> # A tibble: 11 × 8
#>       age xname yname zname zref                        x     y      z
#>     <dbl> <chr> <chr> <chr> <chr>                   <dbl> <dbl>  <dbl>
#>  1 0.0849 age   hgt   hgt_z nl_2012_hgt_female_27  0.0849 38    -0.158
#>  2 0.167  age   hgt   hgt_z nl_2012_hgt_female_27  0.167  43.5   0.047
#>  3 0      age   wgt   wgt_z nl_2012_wgt_female_27  0       0.99  0.19 
#>  4 0.0849 age   wgt   wgt_z nl_2012_wgt_female_27  0.0849  1.25 -0.203
#>  5 0.167  age   wgt   wgt_z nl_2012_wgt_female_27  0.167   2.1   0.015
#>  6 0.0849 age   hdc   hdc_z nl_2012_hdc_female_27  0.0849 27    -0.709
#>  7 0.167  age   hdc   hdc_z nl_2012_hdc_female_27  0.167  30.5  -0.913
#>  8 0.0849 age   bmi   bmi_z nl_1997_bmi_female_nl  0.0849  8.66 -5.72 
#>  9 0.167  age   bmi   bmi_z nl_1997_bmi_female_nl  0.167  11.1  -3.77 
#> 10 0.0849 hgt   wfh   wfh_z nl_2012_wfh_female_   38       1.25 -0.001
#> 11 0.167  hgt   wfh   wfh_z nl_2012_wfh_female_   43.5     2.1   0.326
```

Column `age` holds decimal age for the measurement. Every row contains a
measurement `yname`, the conditioning variable `xname` and the Z-score
`zname`. The column named `zref` holds the name of the growth reference
(as defined in the `nlreference` package) used to calculate the Z-score.
Columns `y`, `x` and `z` store their values, respectively.

The `persondata()` function extracts the person-level information:

``` r
persondata(tgt)
#> # A tibble: 1 × 16
#>      id name      dob        dobf       dobm       src   dnr   sex     gad    ga
#>   <int> <chr>     <date>     <date>     <date>     <chr> <chr> <chr> <dbl> <dbl>
#> 1    -1 fa308134… 2018-10-11 1995-07-04 1990-12-02 1234  <NA>  fema…   189    27
#> # … with 6 more variables: smo <dbl>, bw <dbl>, hgtm <dbl>, hgtf <dbl>,
#> #   agem <dbl>, etn <chr>
```

The result of `read_bds()` feeds into further data processing in `R`.

## Breakdown in steps

### JSON Input Data

The example file `maria2.json` contains Maria’s data coded in JSON
format according to BDS-schema file
[bds_v2.0.json](https://raw.githubusercontent.com/growthcharts/bdsreader/master/inst/schemas/bds_v2.0.json).
Here’s the contents of the file with the child data:

    {
      "Format": "2.0",
      "OrganisatieCode": 1234,
      "Referentie": "Maria2",
      "ClientGegevens": [
        {
          "ElementNummer": 19,
          "Waarde": "2"
        },
        {
          "ElementNummer": 20,
          "Waarde": "20181011"
        },
        {
          "ElementNummer": 82,
          "Waarde": 189
        },
        {
          "ElementNummer": 91,
          "Waarde": "2"
        },
        {
          "ElementNummer": 110,
          "Waarde": 990
        },
        {
          "ElementNummer": 238,
          "Waarde": 1670
        },
        {
          "ElementNummer": 240,
          "Waarde": 1900
        },
        {
          "GenesteElementen": [
            {
              "ElementNummer": 63,
              "Waarde": "19950704"
            },
            {
              "ElementNummer": 71
            },
            {
              "ElementNummer": 62,
              "Waarde": "01"
            }
          ]
        },
        {
          "GenesteElementen": [
            {
              "ElementNummer": 63,
              "Waarde": "19901202"
            },
            {
              "ElementNummer": 71
            },
            {
              "ElementNummer": 62,
              "Waarde": "02"
            }
          ]
        }
      ],
      "ContactMomenten": [
        {
          "Tijdstip": "20181011",
          "Elementen": [
            {
              "ElementNummer": 245,
              "Waarde": 990
            }
          ]
        },
        {
          "Tijdstip": "20181111",
          "Elementen": [
            {
              "ElementNummer": 235,
              "Waarde": 380
            },
            {
              "ElementNummer": 245,
              "Waarde": 1250
            },
            {
              "ElementNummer": 252,
              "Waarde": 270
            }
          ]
        },
        {
          "Tijdstip": "20181211",
          "Elementen": [
            {
              "ElementNummer": 235,
              "Waarde": 435
            },
            {
              "ElementNummer": 245,
              "Waarde": 2100
            },
            {
              "ElementNummer": 252,
              "Waarde": 305
            }
          ]
        }
      ]
    }

JSON is a lightweight format to exchange data between electronic
systems. `"ElementNummer"` fields refer to the numbers defined in the
Basisdataset JGZ, whereas `"Waarde"` fields contain the value. The
element numbers and the value correspond to the Basisdataset JGZ, which
you can find
[here](https://www.ncj.nl/themadossiers/informatisering/basisdataset/documentatie/).

### Read and parse input data

### Validate input data

### Age calculation

### D-score calculation

### Z-score calculation

### Structure of result

## Further information
