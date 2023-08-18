#' Utility table for conversion bds --> lex_gsed
#'
#' @format A data frame
#' \describe{
#' \item{lex_gsed}{GSED item name}
#' \item{bds}{BDS number}
#' \item{bdsr}{BDS number for right}
#' \item{bdsl}{BDs number for left}
#' \item{type}{g1:direct, g2: direct-right/left, m1:message,
#' m2: message-right/left, gr: direct-right, gl: direct-left,
#' mr:message-right, ml:message-left}
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

#' Table 34: Landcodes
#'
#' @docType data
#' @format A `data.frame` with 296 rows and 2 variables:
#' \describe{
#' \item{code}{Country code}
#' \item{country}{Country name}
#' }
#' @source publicaties.rvig.nl, Landelijke_tabellen
"table34"

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

#' Minidata, infant growth of 0-2 years, three children
#'
#'Longitudinal height, weight, head circumference measurements
#'during ages 0-2 years for a representative sample of 3
#'Dutch children born in 1988-1989, including covariates.
#'For demo purposes.
#'
#'@name minidata
#'@docType data
#'@format A list with two components:
#'\describe{
#'\item{child}{Tibble with 14 columns containing child-level covariates}
#'\item{time}{Tibble with 78 columns containing growth data and developmental
#'milestones per time point.}
#'}
#'The \code{child} tibble contains the following variables:
#'\describe{
#'\item{id}{ID, unique \code{id} of each child (numeric)}
#'\item{src}{Source, here \code{"smocc"} (character)}
#'\item{dob}{Data of birth \code{"smocc"} (character)}
#'\item{sex}{Sex, \code{"male"} or \code{"female"} (character)}
#'\item{etn}{Etnicity, \code{"MA"}, \code{"NL"}, or \code{"TU"} (character)}
#'\item{edu}{Educational level (typically mother), \code{"low"},
#' \code{"middle"}, \code{"high"} (character)}
#'\item{ga}{Gestational age in completed, weeks (numeric)}
#'\item{bw}{Birth weight in grammes (numeric)}
#'\item{twin}{Twin, 0 = no, 1 = yes (numeric)}
#'\item{smo}{Mother smoked during pregnancy, 0 = no, 1 = yes (numeric)}
#'\item{agem}{Mother age when giving birth (years) (numeric)}
#'\item{hgtm}{Height of mother in cm (numeric) (numeric)}
#'\item{hgtf}{Height of father in cm (numeric) (numeric)}
#'\item{name}{Child nickname}
#'}
#'
#'The \code{time} tibble contains the following variables:
#'\describe{
#'\item{src}{Source, here \code{"smocc"}}
#'\item{id}{ID, unique \code{id} of each child}
#'\item{rec}{Record number}
#'\item{nrec}{Number of child records, 6+}
#'\item{dob}{Data of birth (character)}
#'\item{dob}{Data of measurement (character)}
#'\item{age}{Decimal age}
#'\item{sex}{Sex, \code{"male"} or \code{"female"}}
#'\item{etn}{Etnicity, \code{"MA"}, \code{"NL"}, or \code{"TU"}}
#'\item{ga}{Gestational age in completed weeks}
#'\item{bw}{Birth weight in grammes}
#'\item{hgt}{Height measurement in cm}
#'\item{wgt}{Weight measurement in kg}
#'\item{hdc}{Head circ measurement in cm}
#'\item{dsc}{D-score, D-unit}
#'\item{bds879}{DDI milestone, BDS number 879}
#'\item{bds...}{DDI milestone, BDS number ...}
#'}
#'@source Herngreen WP, van Buuren S, van Wieringen JC, Reerink JD,
#'Verloove-Vanhorick SP, Ruys JH (1994). Growth in length and weight from
#'birth to 2 years of a representative sample of Netherlands children
#'(born in 1988-89) related to socio-economic status and other background
#'characteristics. \emph{Annals of Human Biology}, \bold{21}, 449-463.
#'@note This dataset is property of the Netherlands Organisation for Applied
#'Scientific Research TNO. Distribution is not permitted.
#'Inquiries at \email{stef.vanbuuren@@tno.nl}.
#'@keywords datasets
NULL
