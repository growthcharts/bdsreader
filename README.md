
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsreader

<!-- badges: start -->

[![R-CMD-check](https://github.com/growthcharts/bdsreader/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/growthcharts/bdsreader/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `bdsreader` package is a lightweight package that

- Defines JSON data schema’s for electronic data exchange of child-level
  data based on the Basisdataset JGZ 4.0.1;
- Reads and parses child data coded according to the JSON data schema;
- Compares the child data against the JSON validation schema;
- Calculates the child’s D-score from the Van Wiechen Schema (DDI)
  responses;
- Adds Z-scores for height, weight, head circumference, BMI,
  weight-for-height and D-score;
- Converts the result into a a list with person-level and time-level
  data for use in JAMES.

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
fn <- system.file("examples", "maria.json", package = "bdsreader")
tgt <- read_bds(fn)
timedata(tgt)
#> # A tibble: 11 × 8
#>       age xname yname zname zref                        x     y      z
#>     <dbl> <chr> <chr> <chr> <chr>                   <dbl> <dbl>  <dbl>
#>  1 0.0849 age   hgt   hgt_z nl_2012_hgt_female_27  0.0849 38    -0.158
#>  2 0.0849 age   wgt   wgt_z nl_2012_wgt_female_27  0.0849  1.25 -0.203
#>  3 0.0849 age   hdc   hdc_z nl_2012_hdc_female_27  0.0849 27    -0.709
#>  4 0.0849 age   bmi   bmi_z nl_1997_bmi_female_nl  0.0849  8.66 -5.72 
#>  5 0.167  age   hgt   hgt_z nl_2012_hgt_female_27  0.167  43.5   0.047
#>  6 0.167  age   wgt   wgt_z nl_2012_wgt_female_27  0.167   2.1   0.015
#>  7 0.167  age   hdc   hdc_z nl_2012_hdc_female_27  0.167  30.5  -0.913
#>  8 0.167  age   bmi   bmi_z nl_1997_bmi_female_nl  0.167  11.1  -3.77 
#>  9 0      age   wgt   wgt_z nl_2012_wgt_female_27  0       0.99  0.19 
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
#>      id name  dob        dobf       dobm       src   dnr   sex      gad    ga
#>   <int> <chr> <date>     <date>     <date>     <chr> <chr> <chr>  <dbl> <dbl>
#> 1    -1 Maria 2018-10-11 1995-07-04 1990-12-02 1234  <NA>  female   189    27
#> # ℹ 6 more variables: smo <int>, bw <dbl>, hgtm <dbl>, hgtf <dbl>, agem <dbl>,
#> #   etn <chr>
```

The result of `read_bds()` feeds into further data processing in `R`.

## Breakdown in steps

### JSON Input Data

The example file `maria3.json` contains Maria’s data coded in JSON
format according to BDS-schema file
[bds_v3.0.json](https://raw.githubusercontent.com/growthcharts/bdsreader/master/inst/schemas/bds_v3.0.json).
Here’s the contents of the file with the child data:

``` javascript
{
  "Format": "3.0",
  "OrganisatieCode": 0,
  "Referentie": "fa308134-069e-49ce-9847-ccdae380ed6f",
  "clientDetails": [
    {
      "bdsNumber": 19,
      "value": "2"
    },
    {
      "bdsNumber": 20,
      "value": "20181011"
    },
    {
      "bdsNumber": 82,
      "value": 189
    },
    {
      "bdsNumber": 91,
      "value": "1"
    },
    {
      "bdsNumber": 110,
      "value": 990
    },
    {
      "bdsNumber": 238,
      "value": 1670
    },
    {
      "bdsNumber": 240,
      "value": 1900
    }
  ],
  "clientMeasurements": [
    {
      "bdsNumber": 235,
      "values": [
        {
          "date": "20181111",
          "value": 380
        },
        {
          "date": "20181211",
          "value": 435
        }
      ]
    },
    {
      "bdsNumber": 245,
      "values": [
        {
          "date": "20181011",
          "value": 990
        },
        {
          "date": "20181111",
          "value": 1250
        },
        {
          "date": "20181211",
          "value": 2100
        }
      ]
    },
    {
      "bdsNumber": 252,
      "values": [
        {
          "date": "20181111",
          "value": 270
        },
        {
          "date": "20181211",
          "value": 305
        }
      ]
    }
  ],
  "nestedDetails": [
    {
      "nestingBdsNumber": 62,
      "nestingCode": "01",
      "clientDetails": [
        {
          "bdsNumber": 63,
          "value": "19950704"
        }
      ],
      "clientMeasurements": [

      ]
    },
    {
      "nestingBdsNumber": 62,
      "nestingCode": "02",
      "clientDetails": [
        {
          "bdsNumber": 63,
          "value": "19901202"
        }
      ],
      "clientMeasurements": [

      ]
    }
  ]
}
```

JSON is a lightweight format to exchange data between electronic
systems. Field `"bdsNumber"` refers to the numbers defined in the
Basisdataset JGZ. Field `"value"` contains the value for the
`"bdsNumber"`. See [Basisdataset
JGZ](https://www.ncj.nl/onderwerp/digitaal-dossier-jgz/basisdatasetjgz-bds/)
4.0.1 for more details on `"bdsNumber"`.

### Read and parse input data

### Validate input data

### Age calculation

### D-score calculation

### Z-score calculation

### Structure of result

## Further information
