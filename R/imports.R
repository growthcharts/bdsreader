#' @importFrom centile         y2z
#' @importFrom dplyr           %>% all_of any_of arrange bind_cols bind_rows
#'                             case_match distinct filter first full_join
#'                             group_by left_join
#'                             mutate num_range pull recode rename select
#'                             slice_head ungroup
#' @importFrom dscore          dscore
#' @importFrom jsonlite        fromJSON prettify toJSON validate
#' @importFrom jsonvalidate    json_validate
#' @importFrom lubridate       NA_Date_ ymd
#' @importFrom nlreferences    set_refcodes
#' @importFrom readr           read_lines
#' @importFrom rlang           .data catch_cnd
#' @importFrom stats           na.omit
#' @importFrom tidyr           drop_na pivot_longer pivot_wider
#' @importFrom tibble          add_column tibble tibble_row
#' @importFrom utils           hasName
NULL

utils::globalVariables(".")
