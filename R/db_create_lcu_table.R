#' Create table with Survey means in Local Currency Units (LCU)
#'
#' @param inv character: vector of survey IDs. If NULL the entire repository
#' will be used
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
db_create_lcu_table <- function(inv = NULL) {

  #--------- add inventory ---------
  if (is.null(inv)) {
    inv <- db_filter_inventory()$survey_id
  }

  #--------- Calculate survey mean LCU ---------

  ld <- purrr::map(.x = inv,
                   .f = lcum)


  #--------- convert to dataframe ---------

  sm <- rbindlist(ld,
                  use.names = TRUE)

  #--------- create components of survey ID ---------

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
     (cnames) := tstrsplit(survey_id, "_", fixed=TRUE)
  ][,
    # create tool and source
    c("tool", "source") := tstrsplit(module, "-", fixed = TRUE)
  ][,
    # change to lower case
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

  setorder(sm, country_code, surveyid_year, module, vermast, veralt)

  return(sm)
}

#' Calculate Survey mean in LCU for one survey
#'
#' @param inv character: survey ID.
#'
#' @return
#' @export
#'
#' @examples
lcum <- function(inv) {

  tryCatch(
    expr = {

      dt <- pipload::pip_load_data(survey_id = inv,
                                   noisy     = FALSE)
      sm <- dt[,
               .(survey_id = unique(survey_id),
                 lcu_mean  = weighted.mean(welfare, weight, na.rm = TRUE)),
               by = .(cpi_data_level, ppp_data_level)
      ]
    }, # end of expr section

    error = function(e) {
      sm <- data.table(survey_id = inv,
                       lcu_mean  = NA)
    } # end of error
  ) # End of trycatch

  return(sm)
}
