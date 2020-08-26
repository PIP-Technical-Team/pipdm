#' @importFrom purrr safely
#' @importFrom data.table data.table
NULL

#' Adjust welfare mean
#'
#' Adjust the welfare mean according to the correct interpolation or
#' extrapolation method.
#'
#' Interpolation and extrapolation of the welfare mean is computed differently
#' depending on various parameters. The adjustment depends on whether surveys
#' are available on one side or both sides of the requested year, the
#' availability of GDP and PCE data as well as the World Bank region the
#' specified country belongs to. This function computes the adjusted mean by
#' applying the specified method.
#'
#' See
#' [Prydz et. al (2019)](http://documents1.worldbank.org/curated/en/664751553100573765/pdf/135460-WP-PUBLIC-Disclosed-3-21-2019.pdf)
#' for more detailed documentation.
#'
#' @param svy_table data.frame: A table with surveys to use for the line-up.
#'   Output of [get_closest_surveys()].
#' @param ref_gdp numeric: GDP value for the reference year.
#' @param ref_pce numeric: PCE value for the reference year.
#' @param method character: A string with the adjustment method to use. Output
#'   of [get_welfare_adjustment_method()].
#'
#' @return `data.table`
#' @references
#' Prydz, E.B, Jolliffe, D., Lakner, C., Mahler, D.G., Sangraula, P. (2019)
#' 'National Accounts Data used in Global Poverty Measurement'.
#' _Global Poverty Monitoring Technical Note_, 8.
#'
#' @keywords internal
adjust_welfare_mean <- function(svy_table, ref_pce, ref_gdp, method) {

  svy_gdp <- svy_table[["adjusted_svy_gdp"]]
  svy_pce <- svy_table[["adjusted_svy_pce"]]
  svy_mean <- svy_table[["svy_year_mean"]]
  svy_year <- svy_table[["survey_year"]]
  region_code <- svy_table[["wb_region_code"]]

  out <- switch(method,
                "one_point_adjusted_with_gdp" =
                  one_point_adjusted_with_gdp(svy_year = svy_year, svy_gdp = svy_gdp,
                                              svy_mean = svy_mean, ref_gdp = ref_gdp),
                "one_point_adjusted_with_pce" =
                  one_point_adjusted_with_pce(svy_year = svy_year, svy_pce = svy_pce,
                                              svy_mean = svy_mean, ref_pce = ref_pce),
                "one_point_same_as_survey_year" =
                  one_point_same_as_survey_year(svy_year = svy_year, svy_mean = svy_mean),
                "non_monotonic_adjusted_with_pce" =
                  non_monotonic_adjusted_with_pce(svy_year = svy_year, svy_pce = svy_pce,
                                                  svy_mean = svy_mean, ref_pce = ref_pce),
                "non_monotonic_adjusted_with_gdp" =
                  non_monotonic_adjusted_with_gdp(svy_year = svy_year, svy_gdp = svy_gdp,
                                                  svy_mean = svy_mean, ref_gdp = ref_gdp),
                "same_direction_interpolated_with_pce" =
                  same_direction_interpolated_with_pce(svy_year = svy_year, svy_pce = svy_pce,
                                                       svy_mean = svy_mean, ref_pce = ref_pce),
                "same_direction_interpolated_with_gdp" =
                  same_direction_interpolated_with_gdp(svy_year = svy_year, svy_gdp = svy_gdp,
                                                       svy_mean = svy_mean, ref_gdp = ref_gdp),
                "missing_pce_a" =
                  missing_pce_a(svy_year = svy_year)
  )

  out$svy_year_mean <- svy_mean
  out$wb_region_code <- svy_table[["wb_region_code"]]

  return(out)
}

#' Safe (purrr::safely) variant of adjust_welfare_mean
#'
#' @rdname adjust_welfare_mean
safe_adjust_welfare_mean <-
  purrr::safely(adjust_welfare_mean)

#' Welfare mean adjustment functions
#'
#' @param svy_year numeric: A value with the survey year, including decimal.
#' @param svy_mean numeric: A value with the survey mean.
#' @param svy_gdp numeric: GDP value for the survey year.
#' @param svy_pce numeric: PCE value for the survey year.
#' @param ref_gdp numeric: GDP value for the reference year.
#' @param ref_pce numeric: PCE value for the reference year.
#'

#' missing_pce_a
#' @noRd
missing_pce_a <- function(svy_year) {
  out <- data.table::data.table(survey_year = svy_year, ref_year_mean = NA)
  return(out)
}

#' one_point_adjusted_with_gdp
#' @noRd
one_point_adjusted_with_gdp <- function(svy_year, svy_gdp, svy_mean, ref_gdp) {
  tryCatch({
    ref_year_mean <- ref_gdp / svy_gdp * svy_mean
    out <- data.table::data.table(survey_year = svy_year, ref_year_mean)
    return(out)
  },
  error = function(e) e)
}

#' one_point_adjusted_with_pce
#' @noRd
one_point_adjusted_with_pce <- function(svy_year, svy_pce, svy_mean, ref_pce) {
  tryCatch({
    ref_year_mean <- ref_pce / svy_pce * svy_mean
    out <- data.table::data.table(survey_year = svy_year, ref_year_mean)
    return(out)
  },
  error = function(e) e)
}

#' one_point_same_as_survey_year
#' @noRd
one_point_same_as_survey_year <- function(svy_year, svy_mean) {
  out <- data.table::data.table(survey_year = svy_year, ref_year_mean = svy_mean)
  return(out)
}

#' non_monotonic_adjusted_with_pce
#' @noRd
non_monotonic_adjusted_with_pce <- function(svy_year, svy_pce, svy_mean, ref_pce) {
  tryCatch({
    mean1 <- ref_pce / svy_pce[1] * svy_mean[1]
    mean2 <- ref_pce / svy_pce[2] * svy_mean[2]
    ref_year_mean = c(mean1, mean2)
    out <- data.table::data.table(survey_year = svy_year, ref_year_mean)
    return(out)
  },
  error = function(e) e)
}

#' non_monotonic_adjusted_with_gdp
#' @noRd
non_monotonic_adjusted_with_gdp <- function(svy_year, svy_gdp, svy_mean,ref_gdp) {
  tryCatch({
    mean1 <- ref_gdp / svy_gdp[1] * svy_mean[1]
    mean2 <- ref_gdp / svy_gdp[2] * svy_mean[2]
    ref_year_mean <- c(mean1, mean2)
    out <- data.table::data.table(survey_year = svy_year, ref_year_mean)
    return(out)
  },
  error = function(e) e)
}

#' same_direction_interpolated_with_pce
#' @noRd
same_direction_interpolated_with_pce <- function(svy_year, svy_pce, svy_mean, ref_pce) {
  tryCatch({
    ref_year_mean <- (ref_pce - svy_pce[1]) / (svy_pce[2] - svy_pce[1]) * (svy_mean[2] - svy_mean[1]) + svy_mean[1]
    out <- data.table::data.table(survey_year = svy_year, ref_year_mean)
    return(out)
  },
  error = function(e) e)
}

#' same_direction_interpolated_with_gdp
#' @noRd
same_direction_interpolated_with_gdp <- function(svy_year, svy_gdp, svy_mean, ref_gdp) {
  tryCatch({
    ref_year_mean <- (ref_gdp - svy_gdp[1]) / (svy_gdp[2] - svy_gdp[1]) * (svy_mean[2] - svy_mean[1]) + svy_mean[1]
    out <- data.table::data.table(survey_year = svy_year, ref_year_mean)
    return(out)
  },
  error = function(e) e)
}
