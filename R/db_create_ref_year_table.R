# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('gdp', 'pce')
  )

#' Create reference year table
#'
#' Create a table with adjusted welfare means for each reference year.
#'
#' `db_create_ref_year_table()` creates a table for with interpolated or
#' extrapolated welfare means for all specified reference years based on the
#' provided survey, GDP and PCE data.
#'
#' @param gdp_table data.frame: A table with GDP data.
#' @param pce_table data.frame: A table with PCE data.
#' @param pop_table data.frame: A table with population data.
#' @param pfw_table data.frame: A table with the price framework file.
#' @param dsm_table data.frame: A table with survey means.
#' @param ref_years numeric: a vector with reference years.
#' @param pip_years numeric: A vector with calender years used in PIP.
#'
#' @return `data.table`
#' @export
db_create_ref_year_table <- function(gdp_table,
                                     pce_table,
                                     pop_table = NULL, # TBC
                                     pfw_table,
                                     dsm_table,
                                     ref_years,
                                     pip_years) {

  # CHECKS
  check_inputs_ref_years(ref_years)
  check_inputs_pip_years(pip_years)

  ### TO BE REMOVED ONCE WE HAVE SOLVED ###
  ### https://github.com/PIP-Technical-Team/pipaux/issues/7 ###
  # Create Survey Anchor table
  svy_anchor <- db_create_svy_anchor(pfw_table)
  ### END OF REMOVE ###

  # Create National Accounts table
  dt_nac <- db_create_nac_table(gdp_table = gdp, pce_table = pce)
  dt_nac <- dt_nac %>% db_subset_nac_table(pip_years = pip_years)

  # Create a table with survey metadata information and adjusted values
  # for GDP and PCE for surveys that span multiple years
  dt_svy <- db_merge_anchor_nac(nac_table = dt_nac, svy_anchor = svy_anchor)

  # Join with survey mean data
  dt_svy <- merge(dt_svy, dsm_table, all = FALSE,
              by = c('country_code', 'surveyid_year',
                     'survey_acronym', 'data_level'))

  # Create reference year table
  dt_ref <-
    db_create_lkup_table(dt_svy, dt_nac, ref_years) %>% #
    db_get_closest_surveys() %>% # Select closets surveys
    db_select_lineup_surveys %>% # Select lineup surveys
    db_compute_predicted_means %>% # Calculate predicted means
    db_finalize_ref_year_table(svy_anchor) # Finalize table (select rows and columns)

  return(dt_ref)

}
