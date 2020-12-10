#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('survey_id', 'welfare', 'weight', 'cpi_data_level', 'ppp_data_level', '.',
      'lcu_mean', 'survey_id', 'module', 'surveyid_year', 'vermast', 'veralt')
  )

#' Create survey mean table
#'
#' Create a table with welfare means in Local Currency Units (LCU) for each survey.
#'
#' The supplied survey datasets  *must* include the following columns;
#' `survey_id`, `welfare`, `weight`, `cpi_data_level` and `ppp_data_level`.
#'
#' @param dl list: A list with survey datasets. See details.
#'
#' @return data.table
#' @export
db_create_lcu_table <- function(dl) {

  #--------- Calculate survey mean LCU ---------

  dl <- purrr::map(.x = dl, .f = calculate_survey_mean)

  #--------- Convert to data table ---------

  dt <- data.table::rbindlist(dl, use.names = TRUE)

  #--------- Create components of survey ID ---------

  cnames <-
    c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection",
      "module"
    )

  dt[,
     # Name sections of filename into variables
     (cnames) := tstrsplit(survey_id, "_", fixed = TRUE)
  ][,
    # Create tool and source
    c("tool", "source") := tstrsplit(module, "-", fixed = TRUE)
  ][,
    # Change to lower case
    c("vermast", "veralt") := lapply(.SD, tolower),
    .SDcols = c("vermast", "veralt")
  ][,
    # Remove unnecessary variables
    c("M", "A", "collection") := NULL
  ][
    # Remove unnecessary rows
    !(is.na(survey_id))
  ]

  data.table::setorder(
    dt, country_code, surveyid_year,
    module, vermast, veralt)

  return(dt)
}

#' Calculate survey mean
#'
#' Calculate survey mean in local currency units (LCU) for a single survey.
#'
#' @param dt data.table: A survey dataset. See details.
#'
#' @return data.table
#' @keywords internal
calculate_survey_mean <- function(dt) {

  tryCatch(
    expr = {
      # Calculate weighted welfare mean by CPI and PPP
      # data levels for each survey
      dt <- dt[,
               .(survey_id = unique(survey_id),
                 svy_mean_lcu = stats::weighted.mean(welfare, weight, na.rm = TRUE)),
               by = .(cpi_data_level, ppp_data_level)
               ]
      return(dt)
    }, # end of expr section
    error = function(e) {
      return(NULL)
    } # end of error
  ) # End of trycatch

}
