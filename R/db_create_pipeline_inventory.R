#' Create pipeline inventory. This is different from the pip inventory. PIP
#' inventory is the surveys available in the PIP repository. Pipeline inventory
#' takes is the PIP inventory, filtered by the Price FrameWork dataset
#'
#' @param root_dir character: root directory of the PIP data
#' @param maindir character: Main directory
#' @param inv_file character: file path to be loaded.
#'
#' @return
#' @export
#'
db_create_pipeline_inventory <- function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
                                         maindir  = gls$PIP_DATA_DIR,
                                         inv_file = paste0(maindir,
                                                       "_inventory/inventory.fst")
                                         ) {

  pip_inventory <-
    pipload::pip_find_data(
      inv_file     = inv_file,
      filter_to_pc = TRUE,
      maindir      = maindir)

    # Load price framework
  pfw <- pipload::pip_load_aux("pfw", verbose = FALSE)
  pipeline_inventory <-
    db_filter_inventory(dt        = pip_inventory,
                       pfw_table  = pfw)


  return(pipeline_inventory)

}
