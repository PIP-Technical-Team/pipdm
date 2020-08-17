#' Synthetic microdata
#'
#' A dataset containing synthetic microdata for 2000 individuals in a fake
#' country.
#'
#' Note that nothing in this dataset is real. It is only meant for testing and
#' example purposes.
#'
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{country_code}{A fake three letter country code}
#'   \item{survey_year}{The year of the survey}
#'   \item{weight}{The weight of each indvidual's welfare value}
#'   \item{welfare}{The welfare of each indvidual. Can represent either income or consumption.}
#'   \item{survey_coverage}{Geographical location; Urban or Rural}
#'   \item{gender}{Gender; Male or Female}
#' }
#' @source Worldbank internal.
"microdata"
