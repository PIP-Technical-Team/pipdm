#' Title
#'
#' @param cache_ids character:vector with  Cache_ids. default from
#'   `db_create_pipeline_inventory()`
#' @param output_dir character: directory where the cache files are stored. If
#'   could be a patch, like `gls$OUT_SVY_DIR_PC` from
#'   `pipload::add_gls_to_env()` or it could be one of the two shortcuts, "in"
#'   or "out". The former refers to `gls$CACHE_SVY_DIR_PC`. the latter refers to
#'   `gls$OUT_SVY_DIR_PC`.
#' @param verbose logical: whether to display messages
#' @param delete logical or NULL. If Null, menu with choices will be displayed
#'   (default). If false, no file will be deleleted. If TRUE, all old files not
#'   available in cache_id will be deleted
#'
#' @return
#' @export
#'
#' @examples
#' delete_old_output_file(delete = FALSE)
delete_old_output_file <- function(cache_ids = NULL,
                                   output_dir = "in",
                                   verbose    = getOption("pipdm.verbose"),
                                   delete = NULL) {

  if (is.null(cache_ids)) {
    dt <- db_create_pipeline_inventory()
    cache_ids <- dt[, unique(cache_id)]
  }

  if (output_dir == "in") {
    output_dir <- gls$CACHE_SVY_DIR_PC
  }
  if (output_dir == "out") {
    output_dir <- gls$OUT_SVY_DIR_PC
  }


  dir_files <- list.files(path = output_dir,
                          full.names = TRUE)

  file_names <- gsub("(//.+/)|([\\.].+$)", "", dir_files)


  names_delete <- file_names[!(file_names %in% cache_ids)]

  if (length(names_delete) == 0) {
    if (verbose) {
      cli::cli_alert_info("No old files found")
    }
    return(invisible(NA))
  }

  cli::cli_alert_danger("The following files will be deleted?")
  cli::cli_ul(names_delete)

  if (is.null(delete)) {
    delete <- usethis::ui_yeah("Do you want to delete them?")
  }


  if (isTRUE(delete)) {
    to_delete <- dir_files[!(file_names %in% cache_ids)]
    deleted <- file.remove(to_delete)
  } else {
    deleted <- NA
  }

  return(invisible(deleted))
}



