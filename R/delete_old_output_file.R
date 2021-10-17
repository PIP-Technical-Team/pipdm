#' Title
#'
#' @param cache_ids
#' @param output_dir
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
delete_old_output_file <- function(cache_ids,
                                   output_dir = gls$OUT_SVY_DIR_PC,
                                   verbose    = getOption("pipdm.verbose")) {
  dir_files <- list.files(path = gls$OUT_SVY_DIR_PC,
                          full.names = FALSE)

  dir_files <- gsub("([\\.].+$)", "", dir_files)


  to_delete <- dir_files[!(dir_files %in% cache_ids)]



}
