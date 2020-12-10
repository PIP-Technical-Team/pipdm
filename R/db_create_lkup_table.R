#' @importFrom tidyr expand_grid
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('region_code', 'country_code', 'survey_coverage',
      'nac_data_level', 'ppp_data_level', 'cpi_data_level',
      'nac_domain', 'ppp_domain', 'cpi_domain',
      'cpi_data_level_index', 'cpi_domain_index',
      'nac_data_level_index', 'nac_domain_index',
      'ppp_data_level_index', 'ppp_domain_index',
      'reference_year', 'region_code_index',
      'country_code_index', 'data_level_index',
      'domain_index', 'survey_coverage_index',
      'welfare_type_index','reference_year_index',
      'gdp', 'pce')
  )

#' Create reference year look-up table
#'
#' Create a look-up table with information on all reference years and surveys
#' for further data manipulation.
#'
#' `dt` must included information on survey metadata, survey national account
#' values and deflated survey means.
#'
#' @param dt data.table: A table with survey data. See details.
#' @param nac_table data.table: A table with GDP and PCE data. Output of
#'   [db_create_nac_table()].
#' @param ref_years numeric: Vector with reference years.
#'
#' @return data.table
#' @keywords internal
db_create_lkup_table <- function(dt, nac_table, ref_years) {

  # CHECK inputs
  check_inputs_db_class(dt)

  # Add reference year column
  dt <- dt %>% tidyr::expand_grid(reference_year = ref_years)

  # Add index columns
  dt <- dt %>%
    base::transform(
      region_code_index = region_code,
      country_code_index = country_code,
      welfare_type_index = welfare_type,
      survey_coverage_index = survey_coverage,
      nac_data_level_index = nac_data_level,
      ppp_data_level_index = ppp_data_level,
      cpi_data_level_index = cpi_data_level,
      nac_domain_index = nac_domain,
      ppp_domain_index = ppp_domain,
      cpi_domain_index = cpi_domain,
      reference_year_index = reference_year)

  # Merge with GDP and PCE data (left join)
  dt <- merge(dt, nac_table, all.x = TRUE,
              by.x = c('country_code', 'nac_data_level_index',
                       'nac_domain_index', 'reference_year_index'),
              by.y = c('country_code', 'nac_data_level',
                       'nac_domain', 'year'))

  # Order by index columns
  dt <- dt[order(dt$country_code_index, dt$nac_data_level_index,
                 dt$nac_domain_index, dt$survey_coverage_index,
                 dt$welfare_type_index, dt$reference_year_index),]

  # Nest by index columns
  dt <- dt %>%
    tidyfast::dt_nest(
      region_code_index,
      country_code_index,
      nac_data_level_index,
      nac_domain_index,
      ppp_data_level_index,
      ppp_domain_index,
      cpi_data_level_index,
      cpi_domain_index,
      welfare_type_index,
      survey_coverage_index,
      reference_year_index, gdp, pce,
      .key = 'data')

  # Remove reference year rows where both GDP and PCE are missing
  na_check <- is.na(dt$gdp) & is.na(dt$pce)
  if (any(na_check)) {
    msg <- sprintf(
      'Info: %s country-year(s) are missing both GDP and PCE values. These rows were removed.',
      sum(na_check))
    rlang::inform(msg)
    dt <- dt[!na_check,]
  }

  return(dt)

}
