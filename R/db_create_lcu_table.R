#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('survey_id', 'welfare', 'weight', 'cpi_data_level', 'ppp_data_level',
      'gdp_data_level', 'pce_data_level', 'pop_data_level', 'svy_mean_lcu',
      'surveyid_year', 'survey_acronym', 'gd_type', 'welfare_type')
  )

#' Create survey mean table (LCU)
#'
#' Create a table with welfare means in Local Currency Units (LCU) for each
#' survey.
#'
#' @param dl list: A list with survey mean datasets.
#' @param pop_table data.table: A table with population data.
#' @param pfw_table data.table: A table with the Price Framwork.
#'
#' @return data.table
#' @export
db_create_lcu_table <- function(dl, pop_table, pfw_table) {

  # Convert list to data.table
  dt <- data.table::rbindlist(dl, use.names = TRUE)

  #--------- Merge with POP ---------

  # Create nested POP table
  pop_table$pop_domain <- NULL
  pop_nested <- pop_table %>%
    tidyfast::dt_nest(country_code, pop_data_level, .key = 'data')

  # Merge dt with pop_nested (left join)
  dt <- data.table::merge.data.table(
    dt, pop_nested, all.x = TRUE,
    by = c('country_code', 'pop_data_level'))

  # Adjust population values for surveys spanning two calender years
  dt$survey_pop <-
    purrr::map2_dbl(dt$survey_year, dt$data,
                    adjust_aux_values, value_var = 'pop')

  # Remove nested data column
  dt$data <- NULL

  # ---- Merge with PFW ----

    # Select columns
  pfw_table <-
    pfw_table[, c('region_code', 'country_code',
                  'survey_coverage', 'surveyid_year',
                  'survey_acronym', 'reporting_year')]

  # Merge LCU table with PFW (left join)
  pfw_table$surveyid_year <-
    as.integer(pfw_table$surveyid_year)
  dt <- data.table::merge.data.table(
    dt, pfw_table, all.x = TRUE,
    by = c('country_code', 'surveyid_year',
           'survey_acronym'))

  # ---- Finalize table ----

  # Sort rows
  data.table::setorder(dt, country_code, surveyid_year, survey_acronym)

  # Order columns
  data.table::setcolorder(
    dt, c('survey_id', 'country_code', 'surveyid_year', 'survey_acronym',
          'survey_year', 'welfare_type', 'survey_mean_lcu', 'survey_pop'))

  return(dt)
}

