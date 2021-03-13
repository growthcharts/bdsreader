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
