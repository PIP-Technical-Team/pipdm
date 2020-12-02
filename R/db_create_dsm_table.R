#' Deflate survey mean in LCU to PPP
#'
#' @param cpi Data frame with most latest version of CPI
#' @param ppp Data frame with most latest version of PPP
#' @param dt  Updated table with LCU means. If NULL call `db_create_lcu_table`
#' parsing argument `inv`
#' @param inv character: Survey IDs to be parsed to `db_create_table` in case
#' argument `dt` is NULL. Only works is `dt` is NULL.
#' @inheritParams db_filter_inventory
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
db_create_dsm_table <- function(cpi     = NULL,
                                ppp     = NULL,
                                dt      = NULL,
                                inv     = NULL,
                                datadir = getOption("pip.datadir"),
                                pipedir = getOption("pip.pipedir")) {

  #--------- In case CPI or PPP is null ---------

  if (is.null(cpi)) {
    cpi <- pipload::pip_load_aux(measure = 'cpi')
  }

  if (is.null(ppp)) {
    ppp <- pipload::pip_load_aux(measure = 'ppp')
  }

  #--------- in case dt is not provides ---------

  if (is.null(dt)) {

    dt <- db_create_lcu_table(inv = inv)

  } else {

    if (!(is.null(inv))) {
      cli::cli_alert_info("option {.val inv} ignored sinced {.val dt} is not NULL")
    }

  }

  #--------- make sure everything is data.table ---------
  # Make sure everything is in data.table format
  setDT(dt)
  setDT(cpi)
  setDT(ppp)

  #--------- merge CPI ---------
  cpi_keys <- c("country_code", "surveyid_year", "survey_acronym", "cpi_data_level")
  cpi[,
      surveyid_year := as.character(surveyid_year)]

  dt[cpi,
     on = cpi_keys,
     `:=`(
       cpi = i.cpi,
       ccf = i.ccf
     )
  ]

  #--------- merge PPP ---------
  ppp_keys <- c("country_code", "ppp_data_level")
  dt[ppp[ppp_default == TRUE],  # just default values
     on = ppp_keys,
     `:=`(
       ppp = i.ppp
     )
  ]


  #--------- Mean to PPP values ---------
  dt[,
     # dsm_mean := lcu_mean/cpi/ppp/ccf
     dsm_mean := wbpip::deflate_welfare_mean(welfare_mean = lcu_mean,
                                             ppp = ppp,
                                             cpi = cpi)
  ]

  return(dt)

}
