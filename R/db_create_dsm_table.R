#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('surveyid_year', 'i.cpi', 'i.ccf', 'i.ppp', 'cpi_data_level', 'ppp_default',
      'svy_mean_ppp', 'svy_mean_lcu', 'cpi', 'ppp')
  )

#' Create deflated survey mean table
#'
#' Create a table with deflated welfare means for each country and surveyid
#' year.
#'
#' @param lcu_table data.table: A table with newly estimated LCU survey means.
#'   Output of `db_create_lcu_table()`.
#' @param dsm_table data.table: A table with deflated survey means. Output of
#'   `read_dsm()`.
#' @param cpi_table data.table: A table with CPI data.
#' @param ppp_table data.table: A table with PPP data.
#' @param append logical: If TRUE, append to the current DSM table.
#'
#' @return data.table
#' @export
db_create_dsm_table <- function(lcu_table,
                                dsm_table,
                                cpi_table,
                                ppp_table,
                                append = TRUE) {

  # Set dt
  dt <- lcu_table

  #--------- Merge with CPI ---------
  cpi_keys <- c("country_code", "surveyid_year", "survey_acronym", "cpi_data_level")
  cpi_table[,
      surveyid_year := as.character(surveyid_year)]

  dt[cpi_table,
     on = cpi_keys,
     `:=`(
       cpi = i.cpi,
       ccf = i.ccf
     )
  ]

  #--------- Merge with PPP ---------
  ppp_keys <- c("country_code", "ppp_data_level")
  dt[ppp_table[ppp_default == TRUE],  # just default values
     on = ppp_keys,
     `:=`(
       ppp = i.ppp
     )
  ]

  #--------- Deflate welfare mean ---------
  dt[,
     # svy_mean_ppp := svy_mean_lcu/cpi/ppp
     svy_mean_ppp  := wbpip::deflate_welfare_mean(
       welfare_mean = svy_mean_lcu, ppp = ppp, cpi = cpi)
  ]

  #--------- Append to current file ---------

  if (append) {
    dt <- join_dsm_tables(new = dt, old = dsm_table)
  }

  return(dt)
}

#' Append new and old DSM tables
#'
#' @param new data.table: A table with newly estimated deflated survey means.
#' @param old data.table: The current DSM table. Output of `read_dsm()`.
#'
#' @return data.table
#' @keywords internal
join_dsm_tables <- function(new, old) {

  # New ids
  new_id <- new[, .(survey_id = unique(survey_id))]

  # Remove in old in case there is an update
  old <- old[!new_id, on = .(survey_id)]

  # Append data
  dt <- data.table::rbindlist(
    list(new, old),
    use.names = TRUE,
    fill = TRUE)

  # Set order
  data.table::setorder(dt, country_code, surveyid_year,
                       module, vermast, veralt)

  return(dt)

}
