#' Create reference estimation table
#'
#' Merge the interpolated means and distributional statistics tables.
#'
#' @param ref_year_table data.table: Output of [db_create_ref_year_table()].
#' @param dist_table data.table: Output of [db_create_dist_table()].
#'
#' @return data.table
#' @export
db_create_ref_estimation_table <- function(ref_year_table, dist_table) {

  # Merge refyear table w/ dist stat table (left join)
  dist_table$reporting_year <- NULL
  dt <- joyn::merge(ref_year_table, dist_table,
                   by = c('survey_id', 'cache_id', 'wb_region_code',
                          'pcn_region_code', 'country_code', 'survey_acronym',
                          'surveyid_year', 'survey_year',
                          'welfare_type', 'pop_data_level'),
                   match_type = 'm:1', keep = 'left')

  # Set dist stat columns to NA for interpolated surveys
  dist_cols <- c('survey_median_lcu', 'survey_median_ppp',
                 'gini', 'mld', 'polarization', 'decile1',
                 'decile2', 'decile3', 'decile4', 'decile5',
                 'decile6', 'decile7', 'decile8', 'decile9',
                 'decile10')
  dt <- set_int_cols_to_na(dt, dist_cols)

  return(dt)

}

#' set_int_cols_to_na
#' @noRd
set_int_cols_to_na <- function(dt, cols) {

  for (i in seq_along(cols)) {
    dt[[cols[i]]] <- data.table::fifelse(
      dt$estimation_type == 'interpolation',
      NA_real_, dt[[cols[i]]])
  }

  return(dt)
}

