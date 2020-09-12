#' @importFrom dplyr recode
NULL

#' Recode data_level and survey coverage
#'
#' Recodes 'data_level' variables in the auxiliary data to match
#' the 'survey_coverage' variable in the Survey Anchor table.
#'
#' AE: Note this merge should really be done on an other variable in
#' the Survey Anchor table.
#' @noRd
recode_data_level <- function(x){
  dplyr::recode(x, '0' = 'Rural', '1' = 'Urban', '2' = 'National')
}
recode_survey_coverage <- function(x){
  dplyr::recode(x, N = 'National', U = 'Urban', R = 'Rural')
}

#' Find countries without national coverage
#'
#' Check the survey anchor table for countries without any national survey
#' coverage. If any such countries exists the functions returns a character
#' vector with country codes for those countries.
#'
#' @param svy_anchor data.frame: A table with survey metadata information.
#'
#' @seealso `db_merge_anchor_na()` `db_finalize_ref_year_table()`
#' @noRd
check_no_national_survey <- function(svy_anchor){
  tmp <- table(svy_anchor$country_code, svy_anchor$survey_coverage)
  tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
  names(tmp) <- c('country_code', 'survey_coverage', 'freq')
  cc <- tmp[(tmp$survey_coverage == 'National' & tmp$freq == 0), ][['country_code']]
  return(cc)
}

#' Find countries with only one coverage level
#'
#' Check the survey anchor table for countries that only have unique coverage
#' level. This is typically used together with the output from
#' `check_no_national_survey()`. Returns a data frame with the country code
#' and coverage level.
#'
#' @param cc character: A vector with country codes. Output of
#'   \code{check_no_national_survey}.
#' @param svy_anchor data.frame: A table with survey metadata information.
#'
#' @seealso `db_finalize_ref_year_table()`
#' @noRd
find_unique_coverage <- function(cc, svy_anchor){
  x <- unique(svy_anchor[svy_anchor$country_code == cc,]$survey_coverage)
  if (length(x) == 1) {
    out <- data.frame(country_code = cc, survey_coverage = x)
    return(out)
  }
}

