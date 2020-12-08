#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('surveyid_year', 'i.cpi', 'i.ccf', 'i.ppp', 'cpi_data_level', 'ppp_default',
      'dsm_mean')
  )

#' Create deflated survey mean table
#'
#' Create a table with deflated welfare means for each country and surveyid
#' year.
#'
#' @param inv character: Survey IDs to be parsed to `db_create_table()` in case
#'   argument `dt` is NULL. Only works if `dt` is NULL.
#' @param cpi data.table: A table with CPI data.
#' @param ppp data.table: A table with PPP data.
#' @param dt data.table: An updated table with LCU survey means. If NULL calls
#'   `db_create_lcu_table()` parsing argument `inv`.
#' @param append logical: If TRUE, append to current DSM table file and save.
#'   TRUE is default.
#' @inheritParams db_filter_inventory
#'
#' @return data.table
#' @export
db_create_dsm_table <- function(inv      = NULL,
                                cpi      = NULL,
                                ppp      = NULL,
                                dt       = NULL,
                                append   = TRUE,
                                datadir  = getOption("pip.datadir"),
                                pipedir  = getOption("pip.pipedir")) {

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
  data.table::setDT(dt)
  data.table::setDT(cpi)
  data.table::setDT(ppp)

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
     # dsm_mean := lcu_mean/cpi/ppp
     dsm_mean := wbpip::deflate_welfare_mean(welfare_mean = lcu_mean,
                                             ppp = ppp,
                                             cpi = cpi)
  ]

  #--------- Append to current file and save ---------

  if (append == TRUE) {

    if (fs::file_exists(getOption("pip.dsmfile"))) {

      old <- fst::read_fst(getOption("pip.dsmfile"))
      data.table::setDT(old)

    } else {
      old <- NULL
    }

    dt <- join_dsm_tables(new = dt, old = old)
    save_dsm(new_dsm = dt)
  }

  return(dt)
}


#' Append new and old DSM tables
#'
#' @param new data.table: A table from `db_create_dsm_table()`.
#' @param old data.table: A table from `getOption("pip.dsmfile")`.
#'
#' @return data.table
#' @keywords internal
join_dsm_tables <- function(new,
                            old = NULL) {
  data.table::setDT(new)

  if (is.null(old)) {

    df <- new

  } else {

    data.table::setDT(old)
    new_id <- new[, .(survey_id = unique(survey_id))]

    #remove in old in case there is an update
    old <- old[!new_id,
               on = .(survey_id)]

    # append data
    df <- data.table::rbindlist(
      list(new, old),
      use.names = TRUE,
      fill = TRUE)
  }
  data.table::setorder(df, country_code, surveyid_year, module, vermast, veralt)

  return(df)

}

#' Save DSM table
#'
#' Save the deflated survey mean table to multiple locations.
#'
#' @param new_dsm data.table: A data frame containing both the old and newly
#'   estimated DSM tables. Output of `join_dsm_tables()`.
#'
#' @return logical
#' @keywords internal
save_dsm <- function(new_dsm) {

  # time for vintage
  time <- format(Sys.time(), "%Y%m%d%H%M%S")
  attr(new_dsm, "datetime") <- time

  # make sure ingestion pipeline directory exists
  dsm_file     <- getOption("pip.dsmfile")
  dsm_dir      <- gsub("(.*/)([^/]+)", "\\1", dsm_file)
  fstfile      <- gsub("(.*/)([^/]+)", "\\2", dsm_file)
  dsm_vint_dir <- paste0(dsm_dir, "_vintage/")

  if (!fs::dir_exists(dsm_vint_dir)) {
    fs::dir_create(dsm_vint_dir,
                   recurse = TRUE)
  }

  # modify output
  dtafile       <- gsub("\\.fst", ".dta", fstfile)
  basefile      <- gsub("\\.fst", "", fstfile)
  vt_output     <- paste0(basefile, "_",time)

  #--------- Save files ---------

  fst::write_fst(x    = new_dsm,
                 path = dsm_file)

  haven::write_dta(data = new_dsm,
                   path = paste0(dsm_dir, dtafile))

  #vintages and backup
  fst::write_fst(x    = new_dsm,
                 path = paste0(dsm_vint_dir, vt_output, ".fst"))

  haven::write_dta(data = new_dsm,
                   path = paste0(dsm_vint_dir, vt_output, ".dta"))

  cli::cli_alert_info("file {.file {fstfile}} and its vintages
                      have been saved", wrap = TRUE)

  return(invisible(TRUE))
}
