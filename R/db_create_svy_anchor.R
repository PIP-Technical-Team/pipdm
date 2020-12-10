#' Create survey anchor
#'
#' @param pfw_table data.table: The price framework file.
#'
#' @return data.table
#' @keywords internal
db_create_svy_anchor <- function(pfw_table) {

  # Subset columns needed for the lineup
  dt <- pfw_table[, c('region_code', 'country_code', 'surveyid_year',
                      'survey_acronym', 'survey_coverage', 'welfare_type',
                      'survey_year', 'gdp_domain', 'pce_domain', 'cpi_domain',
                      'ppp_domain')]

  # Recode domain columns
  dt$gdp_domain <- dplyr::recode(dt$gdp_domain,
                                 '1' = 'national', '2' = 'urban/rural')
  dt$pce_domain <- dplyr::recode(dt$pce_domain,
                                 '1' = 'national', '2' = 'urban/rural')
  dt$cpi_domain <- dplyr::recode(dt$cpi_domain,
                                 '1' = 'national', '2' = 'urban/rural')
  dt$ppp_domain <- dplyr::recode(dt$ppp_domain,
                                 '1' = 'national', '2' = 'urban/rural')
  # Add nac domain column
  if (identical(dt$gdp_domain, dt$pce_domain)) {
    dt$nac_domain <- dt$gdp_domain
  } else {
    rlang::abort(c('Could not create `nac_domain` column.',
      x = '`gdp_domain` and `pce_domain` are not identical.'))
  }

  return(dt)
}
