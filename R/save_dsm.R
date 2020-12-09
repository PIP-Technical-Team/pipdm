# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables('dsm_file')

#' Save DSM table
#'
#' Save the deflated survey mean table to multiple locations.
#'
#' @param new_dsm data.table: A data frame containing both the old and newly
#'   estimated DSM tables. Output of `join_dsm_tables()`.
#' @param path character: Path to the DSM file.
#'
#' @return logical
#' @export
save_dsm <- function(new_dsm, path) {

  # Set time (for vintage)
  time <- format(Sys.time(), "%Y%m%d%H%M%S")
  attr(new_dsm, "datetime") <- time

  # Make sure ingestion pipeline directory exists
  dsm_dir      <- gsub("(.*/)([^/]+)", "\\1", path)
  fstfile      <- gsub("(.*/)([^/]+)", "\\2", path)
  dsm_vint_dir <- paste0(dsm_dir, "_vintage/")

  if (fs::dir_exists(dsm_vint_dir)) {
    fs::dir_create(dsm_vint_dir, recurse = TRUE)
  }

  # Modify output
  dtafile       <- gsub("\\.fst", ".dta", fstfile)
  basefile      <- gsub("\\.fst", "", fstfile)
  vt_output     <- paste0(basefile, "_", time)

  #--------- Save files ---------

  fst::write_fst(x    = new_dsm,
                 path = dsm_file)

  haven::write_dta(data = new_dsm,
                   path = paste0(dsm_dir, dtafile))

  # Vintages and backup
  fst::write_fst(x    = new_dsm,
                 path = paste0(dsm_vint_dir, vt_output, ".fst"))

  haven::write_dta(data = new_dsm,
                   path = paste0(dsm_vint_dir, vt_output, ".dta"))

  cli::cli_alert_info("file {.file {fstfile}} and its vintages
                      have been saved", wrap = TRUE)

  return(invisible(TRUE))
}


