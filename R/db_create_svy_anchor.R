#' Create survey anchor
#'
#' @param dt data.table: The price framework file.
#'
#' @return `data.table`
#' @keywords internal
db_create_svy_anchor <- function(dt) {

  # Subset surveys in PovcalNet
  dt <- dt[dt$inpovcal == 1,]

  # Select columns and recode column names
  cnames <- c('region_code', 'country_code', 'surveyid_year',
              'survey_name', 'survey_coverage', 'comparability',
              'datatype', 'reference_year', 'rep_year',
              'inpovcal')
  cnames2 <- c('region_code', 'country_code', 'surveyid_year',
               'survey_acronym', 'survey_coverage', 'survey_comparability',
               'welfare_type', 'survey_year', 'reporting_year',
               'inpovcal')
  dt <- dt[, cnames, with = FALSE]
  names(dt) <- cnames2

  # Recode survey coverage
  dt$survey_coverage <-
    dplyr::recode(dt$survey_coverage,
                  N = 'national', U = 'urban', R = 'rural')

  # Recode welfare type
  dt$welfare_type <-
    dplyr::recode(dt$welfare_type,
                  I = 'income', C = 'consumption',
                  i = 'income', c = 'consumption')

  return(dt)
}
