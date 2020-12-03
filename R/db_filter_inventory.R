#' filter repository of those surveys that are already in used in dsm_table
#'
#' @param raw_inventory dataframe from `getOption("pip.inventory")`
#' @param datadir character: PIP-Data directory
#' @param pipedir character: Pipeline ingestion directory
#' @param dsm_file character: dsm table file. Dafault `getOption("pip.dsmfile")`
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
db_filter_inventory <-function(raw_inventory = getOption("pip.inventory"),
                               dsm_file      = getOption("pip.dsmfile"),
                               datadir       = getOption("pip.datadir"),
                               pipedir       = getOption("pip.pipedir")) {

  # Raw Inventory
  ri <- fst::read_fst(raw_inventory)
  setDT(ri)
  ri <- ri[,
           survey_id := gsub("\\.dta", "", filename)
  ][
    toupper(tool) == "PC"
  ]

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
