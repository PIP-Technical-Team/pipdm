#' @importFrom data.table fifelse
NULL

#' Summary statistics
#'
#' Compute summary statistics, including Gini, Lorenz curve, weighted mean and
#' minimum and maximum values.
#'
#' Note that the input parameters cannot contain missing values and that
#' \code{y} must be sorted in increasing order.
#'
#' @param y numeric: A vector of income or consumption values
#' @param w numeric: A vector of weights
#'
#' @return Returns a list of summary statistics.
#' @export
#'
compute_stats <- function(y, w){

  # CHECK that inputs are valid
  check_yw_input(y, w)

  # Define number of points on the Lorenz curve
  nobs <- length(y)
  m <- ifelse(nobs > 1000, 100, 20)

  # Calculate summary statistics
  sumW <- sum(w)
  sumY <- sum(w * y)
  meanY <- stats::weighted.mean(y, w)
  minY <- min(y)
  maxY <- max(y)
  mld <- compute_mld(y, w)
  gini <- compute_gini(y, w)
  lorenz <- compute_lorenz(y, w)

  out <- list(nobs = nobs,
              m = m,
              sumW = sumW,
              sumY = sumY,
              meanY = meanY,
              minY = minY,
              maxY = maxY,
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
#' Given a vector of income or consumption values (\code{y}) and their respective
#' weights (\code{w}) \code{compute_mld} computes the mean log deviation for the
#' distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' \code{y} must be sorted in increasing order.
#'
#' @inheritParams compute_stats
#'
#' @export
#'
compute_mld <- function(y, w){

  # CHECK that inputs are valid
  check_yw_input(y, w)

  # Calculate mld
  weighted_mean_y <- stats::weighted.mean(y, w)
  sum_weights <- sum(w)
  v <- suppressWarnings(
    data.table::fifelse(y > 0,
                        w / sum_weights * log(weighted_mean_y / y),
                        w / sum_weights * log(weighted_mean_y)
    )
  )
  mld <- sum(v)

  return(mld)
}

#' Gini coefficient
#'
#' Compute the Gini coefficient.
#'
#' Given a vector of income or consumption values (\code{y}) and their respective
#' weights (\code{w}) \code{compute_gini} computes the Gini coefficient for the
#' distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' \code{y} must be sorted in increasing order.
#'
#' @inheritParams compute_stats
#'
#' @export
#'
compute_gini <- function(y, w){

  # CHECK that inputs are valid
  check_yw_input(y, w)

  # Calculate Gini
  delta <- w * y
  auc <- 0
  sY <- 0
  for (i in seq_along(y)) {
    auc <- auc + (sY + delta[i] / 2) * w[i]
    sY <- sY + delta[i] # Cumulative income
  }
  gini <- 1 - auc / sum(w) / sum(delta) * 2
  return(gini)
}

#' Lorenz curve
#'
#' Compute the Lorenz curve.
#'
#' Given a vector of income or consumption values (\code{y}) and their respective
#' weights (\code{w}) \code{compute_lorenz} computes the Lorenz curve the
#' distribution.
#'
#' Note that the input parameters cannot contain missing values and that
#' \code{y} must be sorted in increasing order.
#'
#' @inheritParams compute_stats
#'
#' @export
#'
compute_lorenz <- function(y, w){

  # CHECK that inputs are valid
  check_yw_input(y, w)

  m <- ifelse(length(w) > 1000, 100, 20)
  delta <- y * w
  sumY <- sum(delta)
  sumW <- sum(w)
  yStep <- sumW / m
  nextLevel <- yStep
  j <- 1
  sW <- 0
  sY <- 0

  # Compute Lorenz curve
  df <- data.frame(matrix(0, m, 3))
  names(df) <- c('y', 'lorenzW', 'lorenzY')

  for (i in seq_along(y)) {

    sW <- sW + w[i] # Cumulative weight
    sY <- sY + delta[i] # Cumulative income

    while ((sW >= nextLevel) & (j <= m)) {

      df$y[j] <- y[i]
      df$lorenzW[j] <- sW / sumW
      df$lorenzY[j] <- sY / sumY

      j <- j + 1
      if (j <= m) {nextLevel <- (yStep * j) * (0.999999999)} # Why 0.9999?
    }
  }
  return(df)
}

#' Check that y and w inputs are valid.
#'
#' @inheritParams compute_stats
#'
#'@noRd
check_yw_input <- function(y, w){
  # Validation checks
  assertthat::assert_that(!anyNA(w), msg = 'w cannot contain missing values')
  assertthat::assert_that(!anyNA(y), msg = 'y cannot contain missing values')
  assertthat::assert_that(is.numeric(w))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(w) == length(y), msg = 'y and w must be of the same length')
  assertthat::assert_that(!is.unsorted(y), msg = "y must be sorted in increasing order")
}








