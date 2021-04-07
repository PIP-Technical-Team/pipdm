#' Create distributional statistics table
#'
#' Create a table with distributional statistics, including a deflated median.
#'
#' @param dl list: A list with distributional statistics datasets.
#' @param crr_inv data frame with correspondence inventory
#' @inheritParams db_create_ref_year_table
#'
#' @return data.table
#' @export
db_create_dist_table <- function(dl,
                                 dsm_table,
                                 crr_inv) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Prepare data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Checks
  cache_id <- crr_inv$cache_id

  assertthat::assert_that(
    length(dl) == length(cache_id),
    msg = '`dl` and `survey_id` must be of equal lengths.')

  names(dl) <- cache_id

  if (data.table::is.data.table(crr_inv)) {
    crr_inv <- data.table::copy(crr_inv)
  } else {
    crr_inv <- data.table::as.data.table(crr_inv)
  }

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
    purrr::map(tibble::as_tibble)


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


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #           Merges and clean data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## merge quantiles with other dist stats --------
  df <- joyn::merge(qt, ds,
                    by = c("cache_id", "pop_data_level"),
                    match_type = "1:1")

  if (nrow(df[report != "x & y"]) > 0) {
    msg     <- "quantiles and othes-dist-stats tables should have the
    very same observations"
    hint    <- "There is something wrong with the code above"
    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "pipdm_error"
    )

  }

  df[, report := NULL]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Merge with corespondence inventory to get survey_id --------

  df[crr_inv,
     on = "cache_id",
     survey_id := i.survey_id
     ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##           Merge with DSM --------

  dsm_table <-
    dsm_table[, .SD, .SDcols =
                c('survey_id', 'cache_id', 'wb_region_code', 'pcn_region_code',
                  'country_code', 'surveyid_year', 'survey_year',
                  'reporting_year', 'survey_acronym', 'welfare_type',
                  'cpi', 'ppp', 'pop_data_level')]

  # Merge dist stats with DSM (left join)
  dt <- joyn::merge(df, dsm_table,
                    by            = c("cache_id", "pop_data_level"),
                    match_type     = "1:1")

  dy <- dt[report == "y", cache_id]
  dx <- dt[report == "x", cache_id]

  if (nrow(dy) > 0) {
    # NOTE AE: should this be an error to abort or just to notify? I made it to
    # notify
    # NOTE AC: Sorry I don't understand what this code catches.
    cli::cli_rule(left = "error on dist stats in the following")
    cli::cli_ul(dy)
    cli::cli_rule(right = "end")
  }

  if (nrow(dx) > 0) {
    # NOTE AE: should this be an error to abort or just to notify? I made it
    # to abort
    # NOTE AC: The column survey_mean_ppp should not a part of the dist stat table,
    # so that's not really an issue. But if you mean survey_median_lcu, then yes you are
    # right. All observations should have a non-missing median. In that case the problem
    # would lie in db_compute_dist_stats() or in the underlying wbpip functions.

    cli::cli_rule(left = "error on deflated means in the following")
    cli::cli_ul(dx)
    cli::cli_rule(right = "end")

    msg     <- "We should no have surveys without deflated means"
    hint    <- "Check calculation of means in `db_create_dsm_table` or
    `db_create_lcu_table`"

    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "pipdm_error"
    )

  }

  dt <- dt[report == "x & y"
           ][, report := NULL]

  ## ---- Deflate median ----
  data.table::setnames(dt, "median", "survey_median_lcu")

  dt[,
     survey_median_ppp := wbpip::deflate_welfare_mean(
       welfare_mean = survey_median_lcu,
       ppp          = ppp,
       cpi          = cpi)
     ][,
       c("cpi", "ppp") := NULL
       ]


  ## --- Finalize table ----

  # Order columns
  data.table::setcolorder(
    dt,
    c('survey_id', 'cache_id', 'wb_region_code', 'pcn_region_code',
      'country_code', 'survey_acronym', 'surveyid_year',
      'survey_year', 'reporting_year', 'welfare_type',
      'pop_data_level', 'survey_median_lcu',
      'survey_median_ppp'))

  # Sort rows
  data.table::setorder(dt, cache_id)

  return(dt)
}
