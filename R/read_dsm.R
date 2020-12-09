#' Read DSM table
#'
#' Load the current DSM table.
#'
#' @param path character: Path to the current DSM file.
#'
#' @return data.table
#' @export
read_dsm <- function(path) {

  if (fs::file_exists(path)) {
    dt <- fst::read_fst(path)
    data.table::setDT(dt)
  } else {
    rlang::abort(sprintf('The file \'%s\' does not exists.', path))
  }

  return(dt)
}
