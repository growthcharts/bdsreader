#' Utility table for conversion bds --> lex_gsed
#'
#' @format A data frame
#' \describe{
#' \item{lex_gsed}{GSED item name}
#' \item{bds}{BDS number}
#' \item{bdsr}{BDS number for right}
#' \item{bdsl}{BDs number for left}
#' \item{type}{g1:direct, g2: direct-right/left, m1:message, m2: message-right/left, gr: direct-right, gl: direct-left, mr:message-right, ml:message-left}
#' \item{bds_label}{Dutch label}
#' \item{labelEN}{English label}}
#'
#' @source This table is constructed from `dscore::bds_edited.csv` by
#' script `data-raw/R/bds_gsed_table.R`
#' @keywords dataset
"bds_gsed"

#' BDS lexicon
#'
#' This table defines the labels and units expected from selected BDS entries.
#' Thie table is used to match BDS numbers to their definition and accepted
#' input.
#' @name bds_lexicon
#' @format A data frame
#' \describe{
#' \item{bdsnummer}{BDS number}
#' \item{description}{Variable label - Dutch}
#' \item{description_EN}{Variable label - English}
#' \item{expected}{Expected type of unit}}
#' @keywords dataset
"bds_lexicon"

#' Test child maria, donordata format
#'
#' Example file with child and time data
#' @name maria
#' @format donordata format. A list with elements `child` and `time`.
#'\describe{
#'\item{child}{Tibble containing child-level covariates}
#'\item{time}{Tibble containing growth data per time point.}
#'}
#'The \code{child} tibble contains the following variables:
#'\describe{
#'\item{src}{Source, here \code{"smocc"} (character)}
#'\item{id}{ID, unique \code{id} of each child (numeric)}
#'\item{name}{Child nickname (character)}
#'\item{dob}{Date of birth (character, dd-mm-yy)}
#'\item{sex}{Sex, \code{"male"} or \code{"female"} (character)}
#'\item{gad}{Gestational age in days (numeric)}
#'\item{smo}{Mother smoked during pregnancy, 0 = no, 1 = yes (numeric)}
#'\item{bw}{Birth weight in grammes (numeric)}
#'\item{dobm}{Date of birth, mother (character, dd-mm-yy)}
#'\item{dobf}{Date of birth, father (character, dd-mm-yy)}
#'\item{hgtm}{Height of mother in cm (numeric) (numeric)}
#'\item{hgtf}{Height of father in cm (numeric) (numeric)}
#'\item{etn}{Ethnicity, \code{"MA"}, \code{"NL"}, or \code{"TU"} (character)}
#'}
#'
#'The \code{time} tibble contains the following variables:
#'\describe{
#'\item{src}{Source (character))}
#'\item{id}{ID, unique \code{id} of each child (numeric)}
#'\item{age}{Decimal age (numeric)}
#'\item{sex}{Sex, \code{"male"} or \code{"female"}}
#'\item{ga}{Gestational age in completed weeks}
#'\item{hgt}{Height measurement in cm}
#'\item{wgt}{Weight measurement in kg}
#'\item{hdc}{Head circ measurement in cm}
#'}
#'@keywords datasets
NULL

