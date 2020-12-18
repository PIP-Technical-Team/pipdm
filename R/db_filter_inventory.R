#' Filter inventory
#'
#' Filter the PIP-Data inventory to select surveys to be used in the PIP
#' ingestion pipeline.
#'
#' Selects the latest version of surveys in the Price Framework file (with
#' `inpovcal = 1`) and belong to the Poverty Calculator (PC) module. Also
#' selects the right source for each country-survey_id year (e.g. GPWG is
#' preferred over BIN).
#'
#' @param raw_inventory data.table: A table with the raw inventory. Output of
#'   `read_inventory()`.
#' @param pfw_table data.table: A table with the price framework file.
#'
#' @return data.table
#' @export
db_filter_inventory <- function(raw_inventory, pfw_table) {

  # ---- Filter tool -----

  # Select Poverty Calculator datasets
  dt <- raw_inventory[raw_inventory$tool == 'PC', ]

  # ---- Filter source ----

  # Select by source order;
  # 1) GPWG, 2) HIST, 3) BIN, 4) GROUP, 5) synth
  dt = dt %>%
    tidyfast::dt_nest(country_code, surveyid_year,
                      survey_acronym, vermast, veralt,
                      .key = 'data')
  dt$keep <- purrr::map(dt$data, keep_source)
  dt$data <- NULL
  dt <- dt %>% tidyfast::dt_unnest(keep)

  # ---- Filter version ----

  # Select latest version
  dt <- dt %>%
    tidyfast::dt_nest(country_code, surveyid_year,
                      survey_acronym, module)
  dt$keep <- purrr::map(dt$data, function(x) {
    x <- x[order(x$vermast, x$veralt),]
    x <- x[nrow(x)]
    return(x)
  })
  dt$data <- NULL
  dt <- dt %>% tidyfast::dt_unnest(keep)

  # ---- Filter for surveys in PovcalNet ----

  dt$surveyid_year <-
    as.integer(dt$surveyid_year)

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

#' keep_source
#' Copied from pipload to avoid error in R CMD CHECK.
#' @noRd
keep_source <- function(df){

  source_order <- c("GPWG", "HIST", "BIN", "GROUP", "synth")
  source_avail <- df[, unique(source)]

  out         <- FALSE
  i           <- 0
  maxi        <- length(source_order)
  source_keep <- NULL
  while(out == FALSE && i <= maxi) {

    i <- i + 1
    if (source_order[i] %in% source_avail) {
      source_keep <- source_order[i]
      out         <- TRUE
    }

  }

  if (!is.null(source_keep)) {
    df <- df[source == (source_keep)]
  }
  return(df)
}
