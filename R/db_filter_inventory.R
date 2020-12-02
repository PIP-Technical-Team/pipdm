#' filter repository of those surveys that are already in used in dsm_table
#'
#' @param raw_inventory dataframe from `paste0(datadir, "_inventory/inventory.fst")`
#' @param datadir character: PIP-Data directory
#' @param pipedir character: Pipeline ingestion directory
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
db_filter_inventory <- function(raw_inventory = NULL,
                                datadir       = getOption("pip.datadir"),
                                pipedir       = getOption("pip.pipedir")) {

  # Raw Inventory
  if (is.null(raw_inventory)) {
    raw_inventory <-   fst::read_fst(paste0(datadir, "_inventory/inventory.fst"))
  }

  ri <- data.table::as.data.table(raw_inventory)
  ri <- ri[,
           survey_id := gsub("\\.dta", "", filename)
  ][
    toupper(tool) == "PC"
  ]

  dsm_file <- paste0(pipedir, "dsm/deflated_svy_means.fst")

  if (fs::file_exists(dsm_file)) {
    # Inventory in Use
    csdm <- fst::read_fst(dsm_file)
    setDT(csdm)

    iu <- csdm[, "survey_id"]

    # Get only those that are not in use
    ni <- ri[!iu,
             on = .(survey_id)]

  } else {

    # If deflated svy file does not exist use the whole raw inventory
    ni <- ri
  }


  # To DELETE
  # ni <- ni[country_code %chin% c("HND", "PER", "PRY", "KGZ", "AGO", "POL")]
  # ni <- ni[country_code %chin% c("CHL")]

  return(ni)

}
