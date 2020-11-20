#' Calculate predicted means
#'
#' Predict welfare means for all reference years.
#'
#' @param dt data.table: Output of [db_get_closest_surveys()].
#'
#' @return `data.table`
#' @keywords internal
db_compute_predicted_means <- function(dt) {

  # CHECK inputs
  check_inputs_db_class(dt)

  # Get proxy values
  proxy <-
    purrr::pmap(list(
      svy_table = dt$svy_lineup_items,
      region_code = dt$region_code_index,
      ref_gdp = dt$gdp,
      ref_pce = dt$pce),
      select_proxy)

  # Get survey means
  means <- purrr::map(dt$svy_lineup_items, .f = function(x) x$svy_mean_ppp)

  # Calculate predicted request means
  out <- purrr::map2(means, proxy, wbpip:::compute_predicted_mean)

  # Add predicted means to the survey data tables
  dt$req_items <-
    purrr::map2(dt$svy_lineup_items, out,
                .f = function(x, y) transform(x, pred_mean_ppp = y))

  # Remove 'svy_lineup_items'
  dt$svy_lineup_items <- NULL

  return(dt)
}
