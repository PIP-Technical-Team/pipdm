#' Create survey estimation table
#'
#' Merge the survey mean and distributional statistics tables.
#'
#' @inheritParams db_create_ref_year_table
#' @param dist_table data.table: Output of [db_create_dist_table()].
#' @return data.table
#' @export
db_create_svy_estimation_table <- function(dsm_table, dist_table) {

  dist_table$reporting_year <- NULL
  dist_table$problem <- NULL

  # Merge DSM table w/ dist stat table (full join)
  dt <- joyn::merge(dsm_table, dist_table, match_type = '1:1')
  dt$report <- NULL

  return(dt)

}
