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
#' @param raw_inventory data.table: A table with the raw inventory. Output of
#'   `read_inventory()`.
#' @param dsm_table data.table: A table with deflated survey means. Output of
#'   `read_dsm()`.
#'
#' @return data.table
#' @export
db_filter_inventory <- function(raw_inventory, dsm_table) {

  # Select .dta files belonging to the
  # Poverty Calculator (PC) module
  raw_inventory <- raw_inventory[,
           survey_id := gsub("\\.dta", "", filename)
  ][
    toupper(tool) == "PC"
  ]

  # Inventory in use
  in_use <- dsm_table[, "survey_id"]

  # Get only those that are not in use
  not_in_use <- raw_inventory[!in_use, on = .(survey_id)]

  return(not_in_use)

}
