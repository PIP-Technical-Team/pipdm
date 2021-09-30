#' Update correspondence inventory file
#'
#' @inheritParams create_cache_file
#' @param save logical. If true, it saves, if FALSE it loads the data
#' @param load logical. If true, loads data. if False return TRUE invisibly
#' @return TRUE if file is update. FALSE If no data is in directory
#' @export
pip_update_cache_inventory <-

  function(pipeline_inventory = NULL,
           pip_data_dir       = gls$PIP_DATA_DIR,
           cache_svy_dir      = NULL,
           tool               = c("PC", "TB"),
           save               = TRUE,
           load               = FALSE) {


  tool <- match.arg(tool)

  # Cache directory
  if (is.null(cache_svy_dir)) {
    if (tool == "PC") {
      cache_svy_dir <- gls$CACHE_SVY_DIR_PC
    } else {
      cache_svy_dir <- gls$CACHE_SVY_DIR_TB
    }
  }

  # Pipeline inventory
  if (is.null(pipeline_inventory)) {
    # Load PIP inventory
    if (tool == "PC") {
      fil <- list(filter_to_pc = TRUE)
    } else {
      fil <- list(filter_to_tb = TRUE)
    }

    pip_inventory <-
      do.call(
        pipload::pip_find_data,
        c(
          inv_file = paste0(pip_data_dir, "_inventory/inventory.fst"),
          fil,
          maindir = pip_data_dir
        )
      )

    # Create pipeline inventory
    pipeline_inventory <-
      db_filter_inventory(
        pip_inventory,
        pfw_table =
          pipload::pip_load_aux(
            measure = "pfw",
            msrdir = paste0(
              pip_data_dir,
              "_aux/", "pfw", "/"
            )
          )
      )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get surveys available in cache dir   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cch <- data.table::data.table(
    cache_file =
      list.files(
        path = cache_svy_dir,
        full.names = TRUE,
        pattern = "\\.fst$"
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
      cli::cli_alert_warning("cache inventory has changed")

      crr <- joyn::merge(cci, crr,
        by            = "cache_id",
        match_type    = "1:1",
        update_values = TRUE,
        reportvar     = FALSE,
        verbose       = FALSE
      )

    } else {
      cli::cli_alert_info("cache inventory has not changed")
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


  }

  if (load) {
    return(crr)
  } else {
    return(invisible(TRUE))
  }

}
