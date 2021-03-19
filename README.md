
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsreader

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `bdsreader` package is a lightweight package that

-   Reads and parses JSON data coded according to the Basisdataset JGZ
    protocol;
-   Validates the data against the JSON schema;
-   Calculates the child’s D-score from the Van Wiechen Schema (DDI)
    responses;
-   Adds Z-scores for height, weight, head circumference, BMI,
    weight-for-height and D-score;
-   Stores the result as a structured `tibble`.

The main use of the `bdsreader` is to translate child data (incoming via
an API request) into a data object useful for `R` processing. The
package is part of Joint Automatic Measurement and Evaluation System
(JAMES) developed by the Netherlands Organisation for Applied Scientific
Research TNO.

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
fn <- system.file("examples", "maria.json", package = "bdsreader")
xyz <- read_bds(fn)
xyz
#> # A tibble: 11 x 8
#>       age xname yname zname zref                        x     y      z
#>     <dbl> <chr> <chr> <chr> <chr>                   <dbl> <dbl>  <dbl>
#>  1 0.0849 age   hgt   hgt_z nl_2012_hgt_female_27  0.0849 38    -0.158
#>  2 0.167  age   hgt   hgt_z nl_2012_hgt_female_27  0.167  43.5   0.047
#>  3 0.0849 age   wgt   wgt_z nl_2012_wgt_female_27  0.0849  1.25 -0.203
#>  4 0.167  age   wgt   wgt_z nl_2012_wgt_female_27  0.167   2.1   0.015
#>  5 0.0849 age   hdc   hdc_z nl_2012_hdc_female_27  0.0849 27    -0.709
#>  6 0.167  age   hdc   hdc_z nl_2012_hdc_female_27  0.167  30.5  -0.913
#>  7 0.0849 age   bmi   bmi_z nl_1997_bmi_female_nl  0.0849  8.66 -5.72 
#>  8 0.167  age   bmi   bmi_z nl_1997_bmi_female_nl  0.167  11.1  -3.77 
#>  9 0.0849 hgt   wfh   wfh_z nl_2012_wfh_female_   38       1.25 -0.001
#> 10 0.167  hgt   wfh   wfh_z nl_2012_wfh_female_   43.5     2.1   0.326
#> 11 0      age   wgt   wgt_z nl_2012_wgt_female_27  0       0.99  0.19
```

Column `age` in the result `xyz` holds decimal age for the measurement.
Every row contains a measurement `yname`, the conditioning variable
`xname` and the Z-score `zname`. The column named `zref` holds the name
of the growth reference (as defined in the `nlreference` package) used
to calculate the Z-score. Columns `y`, `x` and `z` store their values,
respectively.

The `persondata()` function returns the person-level information:

``` r
persondata(xyz)
#> # A tibble: 1 x 13
#>      id name  src   dnr   sex      gad    ga   smo    bw  hgtm  hgtf  agem etn  
#>   <int> <chr> <chr> <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>
#> 1    -1 Maria 1234  <NA>  female   189    27     0   990   167   190    27 NL
```

The result of `read_bds()` feeds into further data processing in `R`.

## Breakdown in steps

### JSON Input Data

The example file `maria.json` contains Maria’s data coded in JSON
format. Here’s the contents of the file:

    {
       "Referentie":"Maria",
       "OrganisatieCode":1234,
       "ClientGegevens":{
          "Elementen":[
             {
                "Bdsnummer":19,
                "Waarde":"2"
             },
             {
                "Bdsnummer":20,
                "Waarde":"20181011"
             },
             {
                "Bdsnummer":82,
                "Waarde":189
             },
             {
                "Bdsnummer":91,
                "Waarde":"1"
             },
             {
                "Bdsnummer":110,
                "Waarde":990
             },
             {
                "Bdsnummer":238,
                "Waarde":1670
             },
             {
                "Bdsnummer":240,
                "Waarde":1900
             }
          ],
          "Groepen":[
             {
                "Elementen":[
                   {
                      "Bdsnummer":63,
                      "Waarde":"19950704"
                   },
                   {
                      "Bdsnummer":71,
                      "Waarde":6030
                   },
                   {
                      "Bdsnummer":62,
                      "Waarde":"01"
                   }
                ]
             },
             {
                "Elementen":[
                   {
                      "Bdsnummer":63,
                      "Waarde":"19901202"
                   },
                   {
                      "Bdsnummer":71,
                      "Waarde":6030
                   },
                   {
                      "Bdsnummer":62,
                      "Waarde":"02"
                   }
                ]
             }
          ]
       },
       "Contactmomenten":[
          {
             "Tijdstip":"20181111",
             "Elementen":[
                {
                   "Bdsnummer":235,
                   "Waarde":380
                },
                {
                   "Bdsnummer":245,
                   "Waarde":1250
                },
                {
                   "Bdsnummer":252,
                   "Waarde":270
                }
             ]
          },
          {
             "Tijdstip":"20181211",
             "Elementen":[
                {
                   "Bdsnummer":235,
                   "Waarde":435
                },
                {
                   "Bdsnummer":245,
                   "Waarde":2100
                },
                {
                   "Bdsnummer":252,
                   "Waarde":305
                }
             ]
          }
       ]
    }

JSON is a lightweight format to exchange data between electronic
systems. `"Bdsnummer"` fields refer to the numbers defined in the
Basisdataset JGZ, whereas `"Waarde"` fields contain the value. You can
find the exact specification [here](here).

### Read and parse input data

### Validate input data

### Age calculation

### D-score calculation

### Z-score calculation

### Structure of result

## Further information
