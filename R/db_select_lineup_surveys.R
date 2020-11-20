#' Select correct line-up surveys
#'
#' Select a single line-up survey when there are multiple line-up surveys
#' available.
#'
#' @param dt data.table: Output of [db_get_closest_surveys()].
#'
#' @seealso [select_lineup_survey()]
#' @return `data.table`
#' @keywords internal
db_select_lineup_surveys <- function(dt){

  # CHECK inputs
  check_inputs_db_class(dt)

  # Fetch surveys to be used for line-up
  dt$svy_lineup_items <-
    purrr::map2(dt$svy_items,
                dt$reference_year_index,
                select_lineup_survey)

  # Remove nested 'svy_items' column
  dt$svy_items <- NULL

  return(dt)
}
