#' Calculate survey means
#'
#' Calculate survey mean in local currency units (LCU) for a single survey
#' dataset.
#'
#' @param dt data.table: A survey dataset.
#'
#' @return data.table
#' @export
db_compute_survey_mean <- function(dt) {

  tryCatch(
    expr = {

      # Get distribution type
      dist_type <- unique(dt$distribution_type)

      # Calculate weighted welfare mean
      dt <- compute_survey_mean[[dist_type]](dt)

      # Order columns
      data.table::setcolorder(
        dt, c('survey_id', 'country_code', 'surveyid_year', 'survey_acronym',
              'survey_year', 'welfare_type', 'svy_mean_lcu'))

      return(dt)

    }, # end of expr section

    error = function(e) {


      return(NULL)

    } # end of error
  ) # End of trycatch

}

#' compute_survey_mean
#'
#' A wrapper for choosing the correct survey mean calculation, depending on the
#' distribution type.
#'
#' @inheritParams db_compute_survey_mean
#' @return data.table
#' @noRd
compute_survey_mean <- list(
  micro = function(x) md_compute_survey_mean(x),
  group = function(x) gd_compute_survey_mean(x),
  aggregate = function(x) gd_compute_survey_mean(x)
)

#' gd_compute_survey_mean
#' @inheritParams db_compute_survey_mean
#' @return data.table
#' @noRd
gd_compute_survey_mean <- function(dt) {

  if (unique(dt$gd_type) == 'T05') {
    dt <-
      dt[, .(survey_id = unique(survey_id),
             country_code = unique(country_code),
             surveyid_year = unique(surveyid_year),
             survey_acronym = unique(survey_acronym),
             survey_year = unique(survey_year),
             welfare_type = unique(welfare_type),
             distribution_type = unique(distribution_type),
             gd_type = unique(gd_type),
             svy_mean_lcu = #stats::weighted.mean
               collapse::fmean(
                 x = welfare, w = weight,
                 na.rm = TRUE)),
         by = .(cpi_data_level, ppp_data_level,
                gdp_data_level, pce_data_level,
                pop_data_level)]
    return(dt)
  } else if (unique(dt$gd_type) %in% c('T01', 'T02')) {
    rlang::warn('Calculation for T01/T02 not implmented yet. Returning NULL.' )
    return(NULL)
  }

}

#' md_compute_survey_mean
#' @inheritParams db_compute_survey_mean
#' @return data.table
#' @noRd
md_compute_survey_mean <- function(dt) {

  # Clean data (remove negative values etc.)
  dt <- md_clean_data(dt, welfare = 'welfare', weight = 'weight')$data

  dt <-
    dt[, .(survey_id = unique(survey_id),
           country_code = unique(country_code),
           surveyid_year = unique(surveyid_year),
           survey_acronym = unique(survey_acronym),
           survey_year = unique(survey_year),
           welfare_type = unique(welfare_type),
           distribution_type = unique(distribution_type),
           gd_type = unique(gd_type),
           svy_mean_lcu =
             collapse::fmean(
               x = welfare, w = weight,
               na.rm = TRUE)),
       by = .(cpi_data_level, ppp_data_level,
              gdp_data_level, pce_data_level,
              pop_data_level)
    ]

  return(dt)

}
