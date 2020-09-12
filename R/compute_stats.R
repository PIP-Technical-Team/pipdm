#' @importFrom data.table fifelse
#' @importFrom dplyr lag
NULL

#' Summary statistics
#'
#' Compute summary statistics.
#'
#' Given a vector of income or consumption values and their respective weights
#' `compute_stats()` calculates the Gini, mean log deviation, Lorenz curve, weighted mean and
#' minimum and maximum values for the distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' `measure` must be sorted in increasing order.
#'
#' @param measure numeric: A vector of income or consumption values.
#' @param weight numeric: A vector of weights.
#'
#' @return list
#' @seealso [compute_gini()] [compute_mld()] [compute_lorenz()]
#'
#' @examples
#' # Simple example
#' compute_stats(measure = 1:100, weight = rep(1, 100))
#'
#' # Microdata example
#' data("microdata")
#' compute_stats(measure = microdata$welfare, microdata$weight)
#'
#' @export
compute_stats <- function(measure, weight){

  # CHECK that inputs are valid
  check_inputs_measure_weight(measure, weight)

  # Calculate summary statistics
  sum_weight <- sum(weight)
  sum_weighted_measure <- sum(measure * weight)
  mean_weighted_measure <- stats::weighted.mean(measure, weight)
  min_measure <- min(measure)
  max_measure <- max(measure)
  mld <- compute_mld(measure, weight)
  gini <- compute_gini(measure, weight)
  lorenz <- compute_lorenz(measure, weight)

  out <- list(nobs = length(measure),
              m = nrow(lorenz),
              sum_weight = sum_weight,
              sum_weighted_measure = sum_weighted_measure,
              mean_weighted_measure = mean_weighted_measure,
              min_measure = min_measure,
              max_measure = max_measure,
              mld = mld,
              gini = gini,
              lorenz = lorenz
              )
  return(out)
}

#' Mean log deviation
#'
#' Compute mean log deviation.
#'
#' Given a vector of income or consumption values and their respective weights
#' `compute_mld()` computes the mean log deviation for the distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' `measure` must be sorted in increasing order.
#'
#' @inheritParams compute_stats
#'
#' @return numeric
#' @seealso [compute_stats()] [compute_gini()] [compute_lorenz()]
#'
#' @examples
#' # Simple example
#' compute_mld(measure = 1:100, weight = rep(1, 100))
#'
#' # Microdata example
#' data("microdata")
#' compute_mld(measure = microdata$welfare, microdata$weight)
#'
#' @export
compute_mld <- function(measure, weight){

  # CHECK that inputs are valid
  check_inputs_measure_weight(measure, weight)

  # Calculate mld
  mean_weighted_measure <- stats::weighted.mean(measure, weight)
  sum_weight <- sum(weight)
  v <- suppressWarnings(
    data.table::fifelse(measure > 0,
                        weight / sum_weight * log(mean_weighted_measure / measure),
                        weight / sum_weight * log(mean_weighted_measure)
    )
  )
  mld <- sum(v)

  return(mld)
}

#' Gini coefficient
#'
#' Compute the Gini coefficient.
#'
#' Given a vector of income or consumption values and their respective weights
#' `compute_gini()` computes the Gini coefficient for the distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' `measure` must be sorted in increasing order.
#'
#' @inheritParams compute_stats
#'
#' @return numeric
#' @seealso [compute_stats()] [compute_mld()] [compute_lorenz()]
#'
#' @examples
#' # Simple example
#' compute_gini(measure = 1:100, weight = rep(1, 100))
#'
#' # Microdata example
#' data("microdata")
#' compute_gini(measure = microdata$welfare, microdata$weight)
#'
#' @export
compute_gini <- function(measure, weight){

  # CHECK that inputs are valid
  check_inputs_measure_weight(measure, weight)

  # Calculate Gini
  delta_measure <- measure * weight
  delta_measure_lag <- dplyr::lag(delta_measure, default = 0)
  v <- (cumsum(delta_measure_lag) + delta_measure / 2) * weight
  auc <- sum(v) # Area below Lorenz curve
  gini <- 1 - auc / sum(weight) / sum(delta_measure) * 2
  return(gini)
}

#' Lorenz curve
#'
#' Compute the Lorenz curve.
#'
#' Given a vector of income or consumption values and their respective weights
#' `compute_lorenz()` computes the Lorenz curve for the distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' `measure` must be sorted in increasing order.
#'
#' @inheritParams compute_stats
#'
#' @return numeric
#' @seealso [compute_stats()] [compute_gini()] [compute_mld()]
#'
#' @examples
#' # Simple example
#' compute_lorenz(measure = 1:100, weight = rep(1, 100))
#'
#' # Microdata example
#' data("microdata")
#' compute_lorenz(measure = microdata$welfare, microdata$weight)
#'
#' @export
compute_lorenz <- function(measure, weight){

  # CHECK that inputs are valid
  check_inputs_measure_weight(measure, weight)

  # Define number of points on the Lorenz curve
  if (length(weight) > 1000) m <- 100 else m <- 20

  # Initialize values
  delta_measure <- measure * weight
  sum_weighted_measure <- sum(delta_measure)
  sum_weight <- sum(weight)
  measure_step <- sum_weight / m
  next_level <- measure_step
  j <- 1
  cum_weight <- 0
  cum_measure <- 0

  # Compute Lorenz curve
  df <- data.frame(matrix(0, m, 3))
  names(df) <- c('measure', 'lorenz_weight', 'lorenz_weighted_measure')

  for (i in seq_along(measure)) {

    cum_weight <- cum_weight  + weight[i] # Cumulative weight
    cum_measure  <- cum_measure + delta_measure[i] # Cumulative income

    while ((cum_weight >= next_level) & (j <= m)) {

      df$measure[j] <- measure[i]
      df$lorenz_weight[j] <- cum_weight / sum_weight
      df$lorenz_weighted_measure[j] <- cum_measure / sum_weighted_measure

      j <- j + 1
      if (j <= m) {next_level <- (measure_step * j) * (0.999999999)} # Why 0.9999?
    }
  }
  return(df)
}

#' Check that measure and weight inputs are valid.
#'
#' @inheritParams compute_stats
#' @return logical
#' @noRd
check_inputs_measure_weight <- function(measure, weight){
  # Validation checks
  assertthat::assert_that(!anyNA(weight), msg = 'weight cannot contain missing values')
  assertthat::assert_that(!anyNA(measure), msg = 'measure cannot contain missing values')
  assertthat::assert_that(is.numeric(weight))
  assertthat::assert_that(is.numeric(measure))
  assertthat::assert_that(length(weight) == length(measure), msg = 'measure and weight must be of the same length')
  assertthat::assert_that(!is.unsorted(measure), msg = "measure must be sorted in increasing order")
}
