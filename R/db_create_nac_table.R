#' Merge GDP and PCE data
#'
#' Create a table with national accounts (NAC) data by merging separate tables
#' for GDP and PCE.
#'
#' Both input tables should contain ISO-3 country codes, calender years and
#' information indicating the data level and domain (e.g national, urban/rural
#' or sub-national) of the variable in question. Note that data levels must be
#' comparable across datasets.
#'
#' @param gdp_table data.table: A table with GDP data.
#' @param pce_table data.table: A table with PCE data.
#' @param pip_years numeric: A vector with calender years used in PIP.
#'
#' @return data.table
#' @keywords internal
db_create_nac_table <- function(gdp_table, pce_table, pip_years){

  # Remove rows with rural / urban data levels
  #gdp_table <- gdp_table[!gdp_table$gdp_data_level %in% c('rural', 'urban')]
  #pce_table <- pce_table[!pce_table$pce_data_level %in% c('rural', 'urban')]

  # Standardize ^_data_level ^_domain column names
  names(pce_table) <- sub('^pce[_]', 'nac_', names(pce_table))
  names(gdp_table) <- sub('^gdp[_]', 'nac_', names(gdp_table))

  # Merge GDP and PCE by country, year, data_level and domain (full join)
  dt <- data.table::merge.data.table(
    gdp_table, pce_table, all = TRUE,
    by = c('country_code', 'year', 'nac_data_level', 'nac_domain'))

  # Subset to only include years used by PIP
  dt <- dt[dt$year %in% pip_years, ]
  nac_table <- dt[dt$year %in% 1979:2019, ]

  return(dt)
}
