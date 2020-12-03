#' Deflate survey mean in LCU to PPP
#'
#' @param inv character: Survey IDs to be parsed to `db_create_table` in case
#' argument `dt` is NULL. Only works is `dt` is NULL.
#' @param cpi Data frame with most latest version of CPI
#' @param ppp Data frame with most latest version of PPP
#' @param dt  Updated table with LCU means. If NULL call `db_create_lcu_table`
#' parsing argument `inv`
#' @param append logical: If TRUE, append to current dsm table file and save.
#' TRUE is default.
#' @inheritParams db_filter_inventory
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
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

  #--------- Append to current file and save ---------

  if (append == TRUE) {

    if (fs::file_exists(getOption("pip.dsmfile"))) {

      old <- fst::read_fst(getOption("pip.dsmfile"))
      setDT(old)

    } else {
      old <- NULL
    }

    dt <- join_dsm_tables(new = dt, old = old)
    save_dsm(new_dsm = dt)
  }

  return(dt)
}


#' Append new and old dsm tables
#'
#' @param new dataframe from `db_create_dsm_table`
#' @param old dataframe in `getOption("pip.dsmfile")`
#'
#' @return data.table
#'
#' @examples
join_dsm_tables <- function(new,
                            old = NULL) {
  setDT(new)

  if (is.null(old)) {

    df <- new

  } else {

    setDT(old)
    new_id <- new[, .(survey_id = unique(survey_id))]

    #remove in old in case there is an update
    old <- old[!new_id,
               on = .(survey_id)]

    # append data
    df <- rbindlist(list(new, old),
                    use.names = TRUE,
                    fill = TRUE)
  }
  setorder(df, country_code, surveyid_year, module, vermast, veralt)

  return(df)

}


#' Save Deflated Survey Mean table
#'
#' @param new_dsm New dataframe joining old and just estimated dsm
#'
#' @return TRUE
#'
#' @examples
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
