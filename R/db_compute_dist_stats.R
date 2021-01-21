#' Calculate distributional statistics
#'
#' Calculate distributional statistics  for a single survey dataset.
#'
#' @param dt data.table: A survey dataset.
#' @param mean numeric: A value with the survey mean.
#' @return list
#' @export
db_compute_dist_stats <- function(dt, mean) {

  tryCatch(
    expr = {

      res <- compute_dist_stats(dt, mean)

      return(res)

    }, # end of expr section

    error = function(e) {

      rlang::warn('Distributional statistics caluclation failed. Returning NULL.')

      return(NULL)

    } # end of error
  ) # End of trycatch

}

#' compute_dist_stats
#' @inheritParams db_compute_dist_stats
#' @return list
#' @noRd
compute_dist_stats <- function(dt, mean) {

  # Get distribution type
  dist_type <- unique(dt$distribution_type)

  # Order by population data level
  data.table::setorder(dt, pop_data_level)

  # Calculate distributional statistics
  if (dist_type == 'micro') {
    pop_level <- unique(dt$pop_data_level)
    # Handle U/R split for micro datasets, e.g. IND 2011
    if (length(mean) == 2 & identical(pop_level, c('rural', 'urban'))) {
      # Split by area
      dt_rural <- dt[dt$area == 'rural']
      dt_urban <- dt[dt$area == 'urban']
      res_rural <- md_dist_stats(dt_rural, mean[1])
      res_urban <- md_dist_stats(dt_urban, mean[2])
      res <- list(rural = res_rural, urban = res_urban)
    } else {
      res <- md_dist_stats(dt, mean)
      res <- list(national = res)
    }
  }
  if (dist_type == 'group') {
    res <- gd_dist_stats(dt, mean)
    res <- list(national = res)
  }
  if (dist_type == 'aggregate') {
    # Split by area
    dt_rural <- dt[dt$area == 'rural']
    dt_urban <- dt[dt$area == 'urban']
    res_rural <- gd_dist_stats(dt_rural, mean[1])
    res_urban <- gd_dist_stats(dt_urban, mean[2])
    res <- list(rural = res_rural, urban = res_urban)
  }
  if (dist_type == 'imputed') {
    # TBD
    rlang::warn('Calculation for imputed distribution type not implemented yet. Returning NULL.' )
    res <- NULL
  }

  return(res)

}

#' md_dist_stats
#' @inheritParams db_compute_dist_stats
#' @return list
#' @noRd
md_dist_stats <- function(dt, mean){
  # Calculate dist stats
  res <- md_compute_dist_stats(
    welfare = dt$welfare,
    weight = dt$weight,
    mean = mean)
  return(res)
}

#' gd_dist_stats
#' @inheritParams db_compute_dist_stats
#' @return list
#' @noRd
gd_dist_stats <- function(dt, mean){
  # Calculate dist stats
  res <- gd_compute_dist_stats(
    welfare = dt$welfare,
    population = dt$weight,
    mean = mean)
  # Select dist stats
  res <- res[c('mean', 'median', 'gini', 'polarization', 'mld', 'deciles')]
  # Rename deciles to quantiles (for comparability with md_compute_dist_stats)
  names(res)[length(res)] <- 'quantiles'
  return(res)
}
