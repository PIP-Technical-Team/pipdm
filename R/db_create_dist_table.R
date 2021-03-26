#' Create distributional statistics table
#'
#' Create a table with distributional statistics, including a deflated median.
#'
#' @param dl list: A list with distributional statistics datasets.
#' @param cache_id character: A vector with cache  ids.
#' @param crr_inv data frame with correspondence inventory
#' @inheritParams db_create_ref_year_table
#'
#' @return data.table
#' @export
db_create_dist_table <- function(dl,
                                 cache_id,
                                 dsm_table,
                                 crr_inv) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Prepare data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Checks
  assertthat::assert_that(
    length(dl) == length(survey_id),
    msg = '`dl` and `survey_id` must be of equal lengths.')

  names(dl) <- cache_id

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Treatment of quantiles   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  quantiles <-
    # create single lists, national, urban, and rural.
    unlist(dl,
           recursive = FALSE,
           use.names = TRUE) %>%
    # Extract quantiles list
    purrr::map(`[[`, "quantiles") %>%
    # create a list with several tibbles for the deciles
    purrr::map(~tibble::tibble(value  =.x,
                decile = c(1:length(.x)))
    )


  qt <-
    # Put everything in a tibble
    tibble::enframe(quantiles,
                    value = "quantiles",
                    name  = "cache_id") %>%

    # Create column for pop_data_level and fix survey name
    dplyr::mutate(pop_data_level = gsub("(.+)(\\.)([a-z]+)", "\\3", cache_id),
                  cache_id       = gsub("(.+)(\\.)([a-z]+)", "\\1", cache_id)
    )  %>%
    # unnest the quantile tibble so that I have only a tibble wit no lists
    tidyr::unnest(quantiles) %>%
    # convert to wide to accommodate to output
    tidyr::pivot_wider(values_from = value,
                names_from = decile,
                names_prefix = "decile")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #            Treatment of other dist stats   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  dsnames <- names(dl[[1]][[1]])
  dsnames <- dsnames[!(dsnames %in% "quantiles")]

  diststats <-
    # create single lists, national, urban, and rural.
    unlist(dl,
           recursive = FALSE,
           use.names = TRUE) %>%
    # Extract quantiles list
    purrr::map(`[`, dsnames) %>%
    # create a list with several tibbles for the deciles
    purrr::map(as_tibble)


  ds <-
    # Put everything in a tibble
    tibble::enframe(diststats,
                    value = "diststats",
                    name  = "cache_id") %>%

    # Create column for pop_data_level and fix survey name
    dplyr::mutate(pop_data_level = gsub("(.+)(\\.)([a-z]+)", "\\3", cache_id),
                  cache_id       = gsub("(.+)(\\.)([a-z]+)", "\\1", cache_id)
    )  %>%
    # unnest the quantile tible so that I have only a tiblle wit no lists
    tidyr::unnest(diststats)

  df <- dplyr::full_join(qt, ds,
                         by = c("cache_id", "pop_data_level")
                         ) %>%
    data.table::as.data.table()

  # ---- Merge with DSM ----

  # Select DSM columns
  dsm_table <-
    dsm_table[, .SD, .SDcols =
                c('survey_id', 'cache_id', 'wb_region_code', 'pcn_region_code',
                  'country_code', 'surveyid_year', 'survey_year',
                  'reporting_year', 'survey_acronym', 'welfare_type',
                  'cpi', 'ppp', 'pop_data_level')]

  # Merge dist stats with DSM (left join)
  dt <- data.table::merge.data.table(
    dt, dsm_table, all.x = TRUE,
    by = c('survey_id', 'pop_data_level'))

  # ---- Deflate median ----

  # survey_median_ppp = median / cpi / ppp
  dt$survey_median_lcu <- dt$median
  dt$median <- NULL
  dt$survey_median_ppp <-
    wbpip::deflate_welfare_mean(
      welfare_mean = dt$survey_median_lcu, ppp = dt$ppp, cpi = dt$cpi)
  dt$cpi <- NULL
  dt$ppp <- NULL

  # --- Finalize table ----

  # Order columns
  data.table::setcolorder(
    dt,
    c('survey_id', 'cache_id', 'wb_region_code', 'pcn_region_code',
      'country_code', 'survey_acronym', 'surveyid_year',
      'survey_year', 'reporting_year', 'welfare_type',
      'pop_data_level', 'survey_median_lcu',
      'survey_median_ppp'))

  # Sort rows
  data.table::setorder(dt, survey_id)

  return(dt)
}
