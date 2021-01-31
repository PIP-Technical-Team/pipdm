#' Compute Lorenz
#'
#' Calculate points on the Lorenz curve for a single survey dataset.
#'
#' Only available for microdata.
#'
#' @inheritParams db_compute_survey_mean
#' @export
db_compute_lorenz <- function(dt){

  dist_type <- unique(dt$distribution_type)
  if (dist_type == 'micro') {
    res <- md_compute_lorenz(
      welfare = dt$welfare, weight = dt$weight
    )
  } else {
    rlang::warn('Pre-calculation of Lorenz curves only implemented for microdata. Returning NULL.')
    res <- NULL
  }

  return(res)

}
