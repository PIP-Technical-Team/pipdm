#' @import data.table
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('filename', 'tool')
  )

#' Filter inventory
#'
#' Filter the PIP-Data inventory for those surveys that are not in the existing
#' DSM table.
#'
#' @param raw_inventory character: Path to the raw inventory. Defaults to
#'   `getOption("pip.inventory")`.
#' @param dsm_file character: Path to the DSM file. Defaults to
#'   `getOption("pip.dsmfile")`
#' @param datadir character: Path to the PIP-Data directory. Defaults to
#'   `getOption("pip.datadir")`.
#' @param pipedir character: Path to the ingestion pipeline directory. Defaults
#'   to `getOption("pip.pipedir")`.
#'
#' @return data.table
#' @export
db_filter_inventory <- function(raw_inventory = getOption("pip.inventory"),
                                dsm_file      = getOption("pip.dsmfile"),
                                datadir       = getOption("pip.datadir"),
                                pipedir       = getOption("pip.pipedir")) {

  # Raw Inventory
  ri <- fst::read_fst(raw_inventory)
  data.table::setDT(ri)
  ri <- ri[,
           survey_id := gsub("\\.dta", "", filename)
  ][
    toupper(tool) == "PC"
  ]

  if (fs::file_exists(dsm_file)) {
    # Inventory in Use
    csdm <- fst::read_fst(dsm_file)
    data.table::setDT(csdm)

    iu <- csdm[, "survey_id"]

    # Get only those that are not in use
    ni <- ri[!iu,
             on = .(survey_id)]

  } else {

    # If deflated svy file does not exist use the whole raw inventory
    ni <- ri
  }

  return(ni)

}
