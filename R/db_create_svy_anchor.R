#' Create survey anchor
#'
#' @param df data.frame: The price framework file.
#'
#' @return `data.table`
#' @keywords internal
db_create_svy_anchor <- function(df) {

  # Subset surveys in PovcalNet
  df <- df[df$inpovcal == 1,]

  # Select columns and recode column names
  cnames <- c('region_code', 'country_code', 'surveyid_year',
              'survey_name', 'survey_coverage', 'comparability',
              'datatype', 'reference_year', 'rep_year',
              # 'cpi_domain', 'cpi_domain_var',
              # 'ppp_domain', 'ppp_domain_var',
              'inpovcal')
  cnames2 <- c('region_code', 'country_code', 'surveyid_year',
               'survey_acronym', 'survey_coverage', 'survey_comparability',
               'welfare_type', 'survey_year', 'reporting_year',
               # 'cpi_domain', 'cpi_domain_var',
               # 'ppp_domain', 'ppp_domain_var',
               'inpovcal')
  df <- df[cnames]
  names(df) <- cnames2

  # Recode survey coverage
  df$survey_coverage <-
    dplyr::recode(df$survey_coverage,
                  N = 'national', U = 'urban', R = 'rural')

  # Recode welfare type
  df$welfare_type <-
    dplyr::recode(df$welfare_type,
                  I = 'income', C = 'consumption',
                  i = 'income', c = 'consumption')

  # Recode c/i for PHL
  df$welfare_type <-
    ifelse(df$country_code == 'PHL', 'consumption', df$welfare_type)

  # Convert to data table
  dt <- data.table::setDT(df)

  return(dt)
}
