
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsreader

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `bdsreader` package is a lightweight package that

-   Reads JSON files with data coded according to the Basisdataset JGZ
    protocol;
-   Validates the data against a JSON schema;
-   Calculates the child’s D-score from individual Van Wiechen Schema
    (DDI) response;
-   Adds Z-scores for height, weight, head circumference, BMI,
    weight-for-height and D-score measurements;
-   Stores the result as a structured `tibble`.

The main use of the `bdsreader` is to translate incoming API request
that contain child data into data objects useful for `R` processing. The
package is part of Joint Automatic Measurement and Evaluation System
(JAMES) as developed by the Netherlands Organisation for Applied
Scientific Research TNO.

## Installation

Install the development version `bdsreader` by

``` r
install.packages("remotes")
remotes::install_github("growthcharts/bdsreader")
```

There is no CRAN release.

## Examples

The following lines illustrate the main use of `bdsreader`.

``` r
library(bdsreader)
fn <- system.file("examples", "client3.json", package = "bdsreader")
read_bds(fn)
#> # A tibble: 11 x 8
#>    xname yname zname       x      y      z    age refcode_z            
#>    <chr> <chr> <chr>   <dbl>  <dbl>  <dbl>  <dbl> <chr>                
#>  1 age   hgt   hgt_z  0.0849  38    -0.158 0.0849 nl_2012_hgt_female_27
#>  2 age   hgt   hgt_z  0.167   43.5   0.047 0.167  nl_2012_hgt_female_27
#>  3 age   wgt   wgt_z  0.0849   1.25 -0.203 0.0849 nl_2012_wgt_female_27
#>  4 age   wgt   wgt_z  0.167    2.1   0.015 0.167  nl_2012_wgt_female_27
#>  5 age   hdc   hdc_z  0.0849  27    -0.709 0.0849 nl_2012_hdc_female_27
#>  6 age   hdc   hdc_z  0.167   30.5  -0.913 0.167  nl_2012_hdc_female_27
#>  7 age   bmi   bmi_z  0.0849  86.6  19.1   0.0849 nl_1997_bmi_female_nl
#>  8 age   bmi   bmi_z  0.167  111.   19.0   0.167  nl_1997_bmi_female_nl
#>  9 hgt   wfh   wfh_z 38        1.25 -0.001 0.0849 nl_2012_wfh_female_  
#> 10 hgt   wfh   wfh_z 43.5      2.1   0.326 0.167  nl_2012_wfh_female_  
#> 11 age   wgt   wgt_z  0        0.99  0.19  0      nl_2012_wgt_female_27
```

The file `client3.json` contains child data coded in JSON format. Here’s
the contents of the file:

    {
       "Referentie":"fa308134-069e-49ce-9847-ccdae380ed6f",
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
                "Waarde":"189"
             },
             {
                "Bdsnummer":91,
                "Waarde":"1"
             },
             {
                "Bdsnummer":110,
                "Waarde":"990"
             },
             {
                "Bdsnummer":238,
                "Waarde":"1670"
             },
             {
                "Bdsnummer":240,
                "Waarde":"1900"
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
                      "Waarde":"6030"
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
                      "Waarde":"6030"
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
                   "Waarde":"380"
                },
                {
                   "Bdsnummer":245,
                   "Waarde":"1250"
                },
                {
                   "Bdsnummer":252,
                   "Waarde":"270"
                }
             ]
          },
          {
             "Tijdstip":"20181211",
             "Elementen":[
                {
                   "Bdsnummer":235,
                   "Waarde":"435"
                },
                {
                   "Bdsnummer":245,
                   "Waarde":"2100"
                },
                {
                   "Bdsnummer":252,
                   "Waarde":"305"
                }
             ]
          }
       ]
    }
