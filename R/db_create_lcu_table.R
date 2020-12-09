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
#' @param svy_id character: A vector of survey IDs.
#'
#' @return data.table
#' @export
db_create_lcu_table <- function(svy_id) {


  #--------- Calculate survey mean LCU ---------

  ld <- purrr::map(.x = svy_id,
                   .f = calculate_survey_mean)


  #--------- Convert to data table ---------

  sm <- data.table::rbindlist(ld,
                              use.names = TRUE)

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

  sm[,

     # Name sections of filename into variables
     (cnames) := tstrsplit(survey_id, "_", fixed = TRUE)
  ][,
    # Create tool and source
    c("tool", "source") := tstrsplit(module, "-", fixed = TRUE)
  ][,
    # Change to lower case
    c("vermast", "veralt") := lapply(.SD, tolower),
    .SDcols = c("vermast", "veralt")
  ][
    ,
    # Remove unnecessary variables
    c("M", "A", "collection") := NULL
  ][
    # Remove unnecessary rows
    !(is.na(survey_id))
  ]

  data.table::setorder(
    sm, country_code, surveyid_year,
    module, vermast, veralt)

  return(sm)
}

#' Calculate survey mean
#'
#' Calculate survey mean in local currency units (LCU) for a single survey.
#'
#' @param svy_id character: Survey ID.
#'
#' @return data.table
#' @keywords internal
calculate_survey_mean <- function(svy_id) {

  tryCatch(
    expr = {

      dt <- pipload::pip_load_data(survey_id = svy_id,
                                   noisy     = FALSE)
      sm <- dt[,
               .(survey_id = unique(survey_id),
                 svy_mean_lcu = stats::weighted.mean(welfare, weight, na.rm = TRUE)),
               by = .(cpi_data_level, ppp_data_level)
      ]
    }, # end of expr section

    error = function(e) {
      sm <- data.table::data.table(
        survey_id = svy_id,
        svy_mean_lcu = NA)
    } # end of error
  ) # End of trycatch

  return(sm)
}
