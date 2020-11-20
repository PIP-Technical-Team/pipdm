#' Get closest surveys for all reference years
#'
#' Retrieve the closest surveys for each reference year.
#'
#' @param dt data.table: Output of [db_create_lkup_table()].
#'
#' @seealso [get_closest_surveys()]
#' @return `data.table`
#' @keywords internal
db_get_closest_surveys <- function(dt){

  # CHECK inputs
  check_inputs_db_class(dt)

  # Create a nested list of survey line-up tables
  dt$svy_items <- purrr::map2(dt$data, dt$reference_year_index, get_closest_surveys)

  # Remove nested 'data' column
  dt$data <- NULL

  return(dt)
}
