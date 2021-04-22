#' Update correspondence inventory file
#'
#' @param pipeline_inventory data.table: Pipeline inventory table.
#' @inheritParams find_new_svy_data
#'
#' @return TRUE if file is update. FALSE If no data is in directory
#' @export
#'
#' @examples
pip_update_cache_inventory <- function(pipeline_inventory = NULL,
                                       cache_svy_dir      = NULL,
                                       tool               = c("PC", "TB")
                                       ) {



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If pipeline inventor not provided   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tool <- match.arg(tool)

  # CAche directory

  if (is.null(cache_svy_dir)) {
    if (tool == "PC") {

      cache_svy_dir <- getOption("pip.cachedir.pc")

    } else {

      cache_svy_dir <- getOption("pip.cachedir.tb")

    }
  }

  # pipeliine inventory
  if (is.null(pipeline_inventory)) {
    # Load PIP inventory
    if (tool == "PC") {

      fil <- list(filter_to_pc = TRUE)

    } else {

      fil <- list(filter_to_tb = TRUE)

    }

    pip_inventory <-
      do.call(pipload::pip_find_data,
               c(
                 inv_file = paste0(getOption("pip.maindir"), '_inventory/inventory.fst'),
                 fil,
                 maindir = getOption("pip.maindir")
                 )
      )

    # Create pipeline inventory
    pipeline_inventory <-
      db_filter_inventory(pip_inventory,
                          pfw_table = pipload::pip_load_aux("pfw")
                          )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get surveys available in cache dir   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cch  <- data.table::data.table(cache_file =
                                   list.files(path       = cache_svy_dir,
                                              full.names = TRUE,
                                              pattern    = "\\.fst$"))

  if (nrow(cch) == 0) {
    cli::cli_alert_warning("There is no data in directory {.field {cache_svy_dir}}\n
                           Cache inventory not created",
                           wrap = TRUE)
    return(invisible(FALSE))
  }

  cch[, cache_id := gsub('(.+/)([^/]+)(\\.fst)', '\\2', cache_file)]
  cch <- cch[!grepl("^_", cache_id)]

  # Filter pipeline inventory and select relevant variables
  cols  <- c("orig", "filename", "survey_id")
  icols <- paste0("i.", cols)
  crr   <- cch[pipeline_inventory,
               on = "cache_id",
               (cols) := mget(icols)
                ][,
                  survey_id := gsub("\\.dta", "", filename)
                ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones

  crr_dir      <- glue::glue("{cache_svy_dir}_crr_inventory/")
  crr_filename <- glue::glue("{crr_dir}crr_inventory")
  crr_vintage  <- glue::glue("{crr_dir}vintage/crr_inventory_{time}")

  # FST
  fst::write_fst(crr, glue::glue("{crr_filename}.fst"))
  fst::write_fst(crr, glue::glue("{crr_vintage}.fst"))

  #dta
  haven::write_dta(crr, glue::glue("{crr_filename}.dta"))
  haven::write_dta(crr, glue::glue("{crr_vintage}.dta"))

  return(invisible(TRUE))
}
