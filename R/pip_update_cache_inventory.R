#' Update correspondence inventory file
#'
#' @inheritParams create_cache_file
#' @param save logical. If true, it saves, if FALSE it loads the data
#' @param load logical. If true, loads data. if False return TRUE invisibly
#' @param verbose logical: Whether to display messages. Default is TRUE
#'
#' @return TRUE if file is update. FALSE If no data is in directory
#' @export
pip_update_cache_inventory <- function(
  pipeline_inventory,
  pip_data_dir,
  cache_svy_dir,
  tool = c("PC", "TB"),
  save = TRUE,
  load = FALSE,
  verbose = FALSE) {

  tool <- match.arg(tool)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get surveys available in cache dir   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cch <- data.table::data.table(
    cache_file =
      list.files(
        path       = cache_svy_dir,
        full.names = TRUE,
        pattern    = "\\.fst$"
      )
  )

  if (nrow(cch) == 0) {
    cli::cli_alert_warning("There is no data in directory {.field {cache_svy_dir}}\n
                           Cache inventory not created",
                           wrap = TRUE
    )
    return(invisible(FALSE))
  }

  cch[, cache_id := gsub("(.+/)([^/]+)(\\.fst)", "\\2", cache_file)]
  cch <- cch[!grepl("^_", cache_id)]

  # Filter pipeline inventory and select relevant variables
  cols <- c("orig", "filename", "survey_id")

  crr <- joyn::merge(cch, pipeline_inventory,
                     by         = "cache_id",
                     match_type = "1:1",
                     keep       = "inner",
                     reportvar  = FALSE,
                     yvars      = cols,
                     verbose    = FALSE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Auxiliary variables for joins --------
  crr[,
      wt := gsub("(.+)_([[:upper:]]+)_([[:upper:]]+$)", "\\2", cache_id)
  ][,
    welfare_type := fcase(wt == "CON", "consumption",
                          wt == "INC", "income",
                          default =  "")
  ][, wt := NULL]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Stamps   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones

  crr_dir      <- glue::glue("{cache_svy_dir}_crr_inventory/")
  crr_filename <- glue::glue("{crr_dir}crr_inventory")
  crr_vintage  <- glue::glue("{crr_dir}vintage/crr_inventory_{time}")

  crr_fst <- glue::glue("{crr_filename}.fst")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Current inventory   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (file.exists(crr_fst)) {

    cci <- fst::read_fst(crr_fst, as.data.table = TRUE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check if data has changed --------

    if (!identical(cci, crr)) {
      if (verbose) {
        cli::cli_alert_warning("cache inventory has changed")
      }

      # Update values with new information
      crr <- joyn::merge(crr, cci,
                         by            = "cache_id",
                         match_type    = "1:1",
                         update_values = TRUE,
                         reportvar     = FALSE,
                         verbose       = FALSE,
                         keep          = "inner"
      )

      # remove information that is not longer necessary
      # crr <- joyn::merge(crr, cch,
      #                    by            = "cache_id",
      #                    match_type    = "1:1",
      #                    verbose       = FALSE,
      #                    keep          = "inner",
      #                    reportvar     = FALSE )

    } else {
      if (verbose) {
        cli::cli_alert_info("cache inventory has not changed")
      }
      save <- FALSE
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (save) {

    # fst
    fst::write_fst(crr, crr_fst)
    fst::write_fst(crr, glue::glue("{crr_vintage}.fst"))

    # dta
    haven::write_dta(crr, glue::glue("{crr_filename}.dta"))
    haven::write_dta(crr, glue::glue("{crr_vintage}.dta"))

    if (isTRUE(verbose)) {
      cli::cli_alert_info("file {.url {crr_fst}} has been updated. You
                          can review it by loading it by typing
                          {.code pipload::pip_load_cache_inventory()}",
                          wrap = TRUE
      )
    }


  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_warning("Cache inventory was {cli::col_red('NOT')} updated",
                             wrap = TRUE)
    }

  }

  if (load) {
    return(crr)
  } else {
    return(invisible(TRUE))
  }

}
