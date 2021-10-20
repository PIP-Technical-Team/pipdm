#' Title
#'
#' @param cache_ids character:vector with  Cache_ids. default from
#'   `db_create_pipeline_inventory()`
#' @param output_dir character: directory where the cache files are stored.
#'   default is `gls$OUT_SVY_DIR_PC` using `pipload::add_gls_to_env()`
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
                                   output_dir = gls$OUT_SVY_DIR_PC,
                                   verbose    = getOption("pipdm.verbose"),
                                   delete = NULL) {

  if (is.null(cache_ids)) {
    dt <- db_create_pipeline_inventory()
    cache_ids <- dt[, unique(cache_id)]
  }

  dir_files <- list.files(path = gls$OUT_SVY_DIR_PC,
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
    choice <- menu(c("nope", "yes", "no no no"), title = "Delete?")
    if (choice == 2) {
      delete <- TRUE
    } else {
      delete <- FALSE
    }
  }


  if (isTRUE(delete)) {
    to_delete <- dir_files[!(file_names %in% cache_ids)]
    deleted <- file.remove(to_delete)
  } else {
    deleted <- NA
  }

  return(invisible(deleted))
}



