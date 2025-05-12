
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdsreader

<!-- badges: start -->

[![R-CMD-check](https://github.com/growthcharts/bdsreader/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/growthcharts/bdsreader/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
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

Install the `jamesdemodata` and `bdsreader` packages as

``` r
install.packages("remotes")
remotes::install_github("growthcharts/jamesdemodata")
remotes::install_github("growthcharts/bdsreader")
```

There is no CRAN release of these packages.

## Examples

`read_bds()` and `write_bds()` are the main functions of the package.

### Reading JSON child data

We work with an example dataset `maria.json`, containing the data of one
child, from the `jamesdemodata` package. Find the filename and show the
first 10 lines:

``` r
fn <- system.file("json", "examples", "maria.json", package = "jamesdemodata")
cat(paste(readLines(fn, n = 10), collapse = "\n"), "\n...")
#> {
#>   "Format": "3.0",
#>   "organisationCode": 1234,
#>   "reference": "Maria",
#>   "clientDetails": [
#>     {
#>       "bdsNumber": 19,
#>       "value": "2"
#>     },
#>     { 
#> ...
```

The following commands illustrate the use of the \`read_bds()\`\`
function:

``` r
library(bdsreader)
child <- read_bds(fn)
class(child)
#> [1] "bdsreader" "list"
```

`read.bds()` returns a `bdsreader` object. We print it as

``` r
child
#> <bdsreader>
#> $persondata:
#> # A tibble: 1 × 17
#>      id name  src   dnr   sex      gad    ga   smo    bw  hgtm  hgtf  agem  blbf
#>   <int> <chr> <chr> <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>
#> 1    -1 Maria 1234  <NA>  female   189    27     1   990   167   190    NA    NA
#> # ℹ 4 more variables: blbm <int>, eduf <int>, edum <int>, par <int>
#> 
#> $timedata:
#> # A tibble: 13 × 8
#>       age xname yname zname zref                        x     y      z
#>     <dbl> <chr> <chr> <chr> <chr>                   <dbl> <dbl>  <dbl>
#>  1 0.0849 age   hgt   hgt_z nl_2012_hgt_female_27  0.0849 38    -0.158
#>  2 0.167  age   hgt   hgt_z nl_2012_hgt_female_27  0.167  43.5   0.047
#>  3 0      age   wgt   wgt_z nl_2012_wgt_female_27  0       0.99  0.19 
#>  4 0.0849 age   wgt   wgt_z nl_2012_wgt_female_27  0.0849  1.25 -0.203
#>  5 0.167  age   wgt   wgt_z nl_2012_wgt_female_27  0.167   2.1   0.015
#>  6 0.0849 age   hdc   hdc_z nl_2012_hdc_female_27  0.0849 27    -0.709
#>  7 0.167  age   hdc   hdc_z nl_2012_hdc_female_27  0.167  30.5  -0.913
#>  8 0      age   bmi   bmi_z nl_1997_bmi_female_nl  0      NA    NA    
#>  9 0.0849 age   bmi   bmi_z nl_1997_bmi_female_nl  0.0849  8.66 -5.72 
#> 10 0.167  age   bmi   bmi_z nl_1997_bmi_female_nl  0.167  11.1  -3.77 
#> 11 0      hgt   wfh   wfh_z nl_2012_wfh_female_   NA       0.99 NA    
#> 12 0.0849 hgt   wfh   wfh_z nl_2012_wfh_female_   38       1.25 -0.001
#> 13 0.167  hgt   wfh   wfh_z nl_2012_wfh_female_   43.5     2.1   0.326
```

The object is a list with two elements: `psn` and `zyx`:

- The `psn` element contains the person-level information;
- The `xyz` element contains time-varying measurements for the child.

See `?init_bdsreader` for more details on the structure of the
`bdsreader` class.

### Writing JSON child data

The `write_bds()` function perform the inverse operation. By default, it
returns minified JSON:

``` r
js <- write_bds(child)
cat(paste(js, collapse = "\n"), "\n...")
#> {"Format":"3.0","organisationCode":0,"reference":"Maria","clientDetails":[{"bdsNumber":19,"value":"2"},{"bdsNumber":20,"value":"20181011"},{"bdsNumber":82,"value":189},{"bdsNumber":91,"value":"1"},{"bdsNumber":110,"value":990},{"bdsNumber":238,"value":1670},{"bdsNumber":240,"value":1900}],"clientMeasurements":[{"bdsNumber":235,"values":[{"date":"20181111","value":380},{"date":"20181211","value":435}]},{"bdsNumber":245,"values":[{"date":"20181011","value":990},{"date":"20181111","value":1250},{"date":"20181211","value":2100}]},{"bdsNumber":252,"values":[{"date":"20181111","value":270},{"date":"20181211","value":305}]}],"nestedDetails":[{"nestingBdsNumber":62,"nestingCode":"01","clientDetails":[{"bdsNumber":63,"value":"19950704"}]},{"nestingBdsNumber":62,"nestingCode":"02","clientDetails":[{"bdsNumber":63,"value":"19901202"}]}]} 
#> ...
```

You can write the data in a pretty format using the `indent` and to a
file using `file` argument.
