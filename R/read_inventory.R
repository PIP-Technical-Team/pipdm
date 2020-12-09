#' Read inventory
#'
#' Load the raw inventory file.
#'
#' @param path character: Path to the inventory file.
#'
#' @return data.table
#' @export
read_inventory <- function(path) {

  if (fs::file_exists(path)) {
    dt <- fst::read_fst(path)
    data.table::setDT(dt)
  } else {
    rlang::abort(sprintf('The file \'%s\' does not exists.', path))
  }

  return(dt)
}
