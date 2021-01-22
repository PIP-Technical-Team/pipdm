#' Create regional population table
#'
#' Create an aggregated population table by region.
#'
#' @param pop_table data.table: A table with population data.
#' @param wb_meta data.table: A table with country metadata from WDI. See
#'   details.
#' @param pip_years numeric: A vector with calender years used in PIP.
#'
#' Use `wbstats::wb_countries()` to retrieve the `wb_meta` table.
#'
#' @return data.table
#' @export
db_create_reg_pop_table <- function(pop_table, wb_meta, pip_years) {

  # Subselect columns
  wb_meta <-
    wb_meta[, c('iso3c', 'region', 'region_iso3c')]

  # Merge POP table w/ WDI meta (left join)
  dt <- data.table::merge.data.table(
    pop_table, wb_meta, by.x = 'country_code',
    by.y = 'iso3c', all.x = TRUE)

  # Remove territories with regional classification
  # i.e "ESH" "GLP" "GUF" "MTQ" "MYT" "REU"
  dt <- dt[!is.na(region)]

  # Add region_code column that matches PFW
  dt$region_code <- dplyr::recode(
    dt$region_iso3c,
    LCN = 'LAC',
    SSF = 'SSA',
    ECS = 'ECA',
    MEA = 'MNA',
    EAS = 'EAP',
    NAC = 'NAC'
  )

  # Aggregate population by region
  dt <- dt[, .(pop = sum(pop)),
           by = .(region_code, year)]

  # Subset to only include years used by PIP
  dt <- dt[dt$year %in% pip_years, ]

  return(dt)

}
