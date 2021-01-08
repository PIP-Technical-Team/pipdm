#' Calculate distributional statistics
#'
#' Calculate distributional statistics  for a single survey dataset.
#'
#' @param dt data.table: A survey dataset.
#' @param mean numeric: A value with the survey mean. Only used for grouped data.
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
#' @keywords internal
compute_dist_stats <- function(dt, mean) {

  dist_type <- unique(dt$distribution_type)
  if (dist_type == 'micro') {
    # Clean data (remove negative values etc.)
    dt <- md_clean_data(
      dt, welfare = 'welfare',
      weight = 'weight')$data
    # Calculate dist stats
    res <- wbpip:::md_compute_dist_stats(
      welfare = dt$welfare, weight = dt$weight)
  }
  if (dist_type == 'group') {
    # Get GD type (1, 2, or 5)
    gd_type <- as.numeric(sub('T0', '', unique(dt$gd_type)))
    # Standardize to type 1
    dt <- gd_clean_data(
      dt, welfare = 'welfare', population = 'weight',
      data_type = gd_type)
    # Compute poverty stats
    res <- wbpip:::gd_compute_pip_stats(
      welfare = dt$welfare, population = dt$population,
      requested_mean = mean, povline = 1.9)
    # Select dist stats
    res <- res[c('mean', 'median', 'gini', 'polarization', 'mld', 'deciles')]
    # Rename deciles to quantiles (for comparability with md_compute_dist_stats)
    names(res)[length(res)] <- 'quantiles'
  }
  if (dist_type == 'aggregate') {
    # By area
    res <- NULL
  }
  if (dist_type == 'imputed') {
    # TBD
    res <- NULL
  }

  return(res)

}


