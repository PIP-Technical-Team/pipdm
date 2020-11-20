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
#' @param gdp_table data.frame: A table with GDP data.
#' @param pce_table data.frame: A table with PCE data.
#'
#' @return `data.table`
#' @keywords internal
db_create_nac_table <- function(gdp_table, pce_table){

  # Standardize ^_data_level ^_domain column names
  names(pce_table) <- sub('^pce[_]', '', names(pce_table))
  names(gdp_table) <- sub('^gdp[_]', '', names(gdp_table))

  # Merge GDP and PCE by country, year, data_level and domain (full join)
  df <- merge(gdp_table, pce_table, all = TRUE,
              by = c('country_code', 'year', 'data_level', 'domain'))

  ### TO BE REMOVED ONCE WE HAVE SOLVED
  ### https://github.com/PIP-Technical-Team/pipaux/issues/8
  # Recode data_level to string
  df$data_level <- dplyr::recode(df$data_level,
                                 '0' = 'rural',
                                 '1' = 'urban',
                                 '2' = 'national')

  # Recode domain to string
  df$domain <- dplyr::recode(df$domain,
                             '1' = 'national',
                             '2' = 'urban/rural',
                             '3' = 'subnational region') # Placeholder for future codes.
  ### END OF REMOVE

  # Convert to data.table
  df <- data.table::as.data.table(df)

  return(df)
}

#' Subset national accounts data
#'
#' Subset the output of `db_create_nac_table()` to only include years used by
#' the Poverty and Inequality Platform (PIP).
#'
#' @param nac_table data.table: Output of [db_create_nac_table()]
#' @param pip_years numeric: A vector with calender years used in PIP.
#'
#' @return `data.table`
#' @keywords internal
db_subset_nac_table <- function(nac_table, pip_years)  {

  nac_table[nac_table$year %in% pip_years, ]

}
