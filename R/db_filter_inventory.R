#' Filter inventory
#'
#' Filter the PIP-Data inventory to select surveys in the Price Framework.
#'
#' @param dt data.table: A table with the inventory.
#' @param pfw_table data.table: A table with the price framework file.
#'
#' @return data.table
#' @export
db_filter_inventory <- function(dt, pfw_table) {

  # ---- Filter for surveys in PovcalNet ----

  # Select columns
  pfw_table <- pfw_table[, c('country_code', 'surveyid_year',
                             'survey_acronym', 'inpovcal')]

  # Merge inventory with PFW (left join)
  dt <- data.table::merge.data.table(
    dt, pfw_table, all.x = TRUE,
    by = c('country_code', 'surveyid_year',
           'survey_acronym'))

  # Select surveys in PovcalNet
  dt <- dt[dt$inpovcal == 1,]

  return(dt)

}
