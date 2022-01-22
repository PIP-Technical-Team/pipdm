#' Create censoring table
#'
#' Create censoring table based on manual inputs recorded in the AUX data, as well as the regional coverage.
#'
#' @param censored list: List with manual provided countries or regions to censor.
#' @param coverage_table data.table: Coverage table. Output of `db_create_coverage_table()`.
#' @param coverage_threshold numeric: Value with the threshold to censor regions based on their coverage (e.g 50 %).
#'
#' @export
db_create_censoring_table <- function(censored,
                                      coverage_table,
                                      coverage_threshold) {

  # Filther by threshold
  dt <- coverage_table[coverage < coverage_threshold]
  data.table::setnames(dt, 'pcn_region_code', 'region_code')


  dt <-
    dt[,
     `:=`(
         statistic = 'all',
         id        = paste0(region_code, "_", reporting_year)
       )
     ][,
       c('region_code', 'reporting_year', 'statistic', 'id')
       ]

  # Add region-years to censor based on coverage
  censored$regions <- data.table::rbindlist(list(censored$regions, dt))

  return(censored)
}
