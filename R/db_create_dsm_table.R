#' @import data.table
NULL

#' Create deflated survey mean table
#'
#' Create a table with deflated welfare means for each country and surveyid
#' year.
#'
#' @param lcu_table data.table: A table with newly estimated LCU survey means.
#'   Output of `db_create_lcu_table()`.
#' @param cpi_table data.table: A table with CPI data.
#' @param ppp_table data.table: A table with PPP data.
#' @return data.table
#' @export
db_create_dsm_table <- function(lcu_table,
                                cpi_table,
                                ppp_table) {


  #--------- Merge with CPI ---------

  # Select CPI columns
  cpi_table <-
    cpi_table[, .SD,
      .SDcols =
        c(
          "country_code", "survey_year", "survey_acronym",
          "cpi_data_level", "cpi"
        )
    ]

  # Merge survey table with CPI (left join)
  dt <- joyn::merge(lcu_table, cpi_table,
    by = c(
      "country_code", "survey_year",
      "survey_acronym", "cpi_data_level"
    ),
    match_type = "m:1"
  )

  if (nrow(dt[report == "x"]) > 0) {
    msg <- "We should not have NOT-matching observations from survey-mean tables"
    hint <- "Make sure CPI table is up to date"
    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "pipdm_error"
    )
  }

  dt <- dt[
    report != "y" # This is unnecessary data in cpi table... should we have it?
  ][, report := NULL]
  # NOTE AE: I just asked Minh about why we have some obs in CPI that we don't use.
  # NOTE AC: Yes, I saw the email thread. So that should be okay. In general my understanding is that there will be
  # some discrepancies between PFW, CPI and DLW.


  #--------- Merge with PPP ---------

  # Select default PPP values
  ppp_table <- ppp_table[ppp_default == TRUE]

  # Select PPP columns
  ppp_table <-
    ppp_table[, .SD,
      .SDcols =
        c("country_code", "ppp_data_level", "ppp")
    ]

  # Merge survey table with PPP (left join)
  jn <- joyn::merge(dt, ppp_table,
    by = c("country_code", "ppp_data_level"),
    match_type = "m:1"
  )

  if (nrow(jn[report == "x"]) > 0) {
    msg <- "We should not have NOT-matching observations from survey-mean tables"
    hint <- "Make sure PPP table is up to date"
    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "pipdm_error"
    )
  }

  # Only CHN, IND and IDN could be left behind for national ppp_data_level
  # Note AC: Not sure why we would need to hardcode CHN, IDN, IND here.
  # This should preferrably be handlded "automatically" by the data level columns.
  # If we want this to be a validation check than I think it should be in the
  # validation repo.
  cdt <- dt[, unique(country_code)]
  cppp <- jn[report == "y", unique(country_code)]

  # if (!identical(c("CHN", "IDN", "IND"), intersect(cdt, cppp))) {
  #
  #   miss <- setdiff(intersect(cdt, cppp), c("CHN", "IDN", "IND"))
  #   extr <- setdiff(c("CHN", "IDN", "IND"), intersect(cdt, cppp))
  #
  #   if (length(miss) > 0) {
  #
  #     cli::cli_alert_danger("{.field {miss}} should be present in 'right'
  #                           observations of joining table.", wrap = TRUE)
  #
  #   }
  #
  #   if (length(extr) > 0) {
  #
  #     cli::cli_alert_danger("{.field {extr}} should be present in 'inner'
  #                           observations of joining table.", wrap = TRUE)
  #
  #     msg     <- "missing countries in resulting table."
  #     hint    <- "All countries in survey-mean should have a corresponding PPP value"
  #     rlang::abort(c(
  #       msg,
  #       i = hint
  #     ),
  #     class = "pipdm_error"
  #     )
  #   }
  # }


  dt <- jn[
    report != "y" # Countries in PPP table for which we don't have data
  ][, report := NULL]

  #--------- Deflate welfare mean ---------

  # svy_mean_ppp = survey_mean_lcu / cpi / ppp
  dt$survey_mean_ppp <-
    wbpip::deflate_welfare_mean(
      welfare_mean = dt$survey_mean_lcu, ppp = dt$ppp, cpi = dt$cpi
    )


  #--------- Add comparable spell ---------

  dl <- split(dt, list(dt$country_code, dt$survey_comparability))
  dl <- lapply(dl, function(x) {
    if (nrow(x) == 1) {
      x$comparable_spell <- x$reporting_year
    } else {
      x$comparable_spell <-
        sprintf(
          "%s - %s",
          x$reporting_year[1],
          x$reporting_year[length(x$reporting_year)]
        )
    }
    return(x)
  })
  dt <- data.table::rbindlist(dl)

  #--------- Finalize table ---------

  # Add is_interpolated column
  dt$is_interpolated <- FALSE

  # Add is_used_for_aggregation column
  # Temporary quick fix for is_used_for_aggregation column,
  # see issue PIP-Technical-Team/TMP_pipeline#14
  dt$is_used_for_aggregation <-
    ifelse(dt$pop_data_level != "national",
      TRUE, FALSE
    )

  # Select and order columns
  dt <- dt[, .SD,
    .SDcols =
      c(
        "survey_id", "cache_id", "wb_region_code", "pcn_region_code",
        "country_code", "survey_acronym", "survey_coverage",
        "survey_comparability", "comparable_spell",
        "surveyid_year", "reporting_year",
        "survey_year", "welfare_type",
        "survey_mean_lcu", "survey_mean_ppp", #' survey_pop',
        "reporting_pop", "ppp", "cpi", "pop_data_level",
        "gdp_data_level", "pce_data_level",
        "cpi_data_level", "ppp_data_level", "reporting_level",
        "distribution_type", "gd_type",
        "is_interpolated", "is_used_for_aggregation"
      )
  ]

  # Add aggregated mean for surveys split by Urban/Rural
  dt <- add_aggregated_mean(dt)

  # Sort rows
  data.table::setorder(dt, survey_id)

  return(dt)
}

#' add_aggregated_mean
#'
#' Compute and add aggregated (national) rows for surveys where the mean
#' calculation is split by urban / rural population level.
#'
#' @param dt data.table: A table with deflated survey means.
#' @return data.table
#' @noRd
add_aggregated_mean <- function(dt) {

  # Select rows w/ non-national pop_data_level
  dt_sub <- dt[pop_data_level != "national", ]

  # Compute aggregated mean (weighted population average)
  dt_agg <-
    dt_sub[, .(
      # survey_id       = unique(survey_id),
      # cache_id        = unique(cache_id),
      wb_region_code = unique(wb_region_code),
      pcn_region_code = unique(pcn_region_code),
      country_code = unique(country_code),
      survey_acronym = unique(survey_acronym),
      survey_coverage = unique(survey_coverage),
      survey_comparability = unique(survey_comparability),
      comparable_spell = unique(comparable_spell),
      surveyid_year = unique(surveyid_year),
      reporting_year = unique(reporting_year),
      survey_year = unique(survey_year),
      welfare_type = unique(welfare_type),
      survey_mean_lcu = collapse::fmean(
        x = survey_mean_lcu,
        w = reporting_pop
      ),
      survey_mean_ppp = collapse::fmean(
        x = survey_mean_ppp,
        w = reporting_pop
      ),
      reporting_pop = collapse::fsum(reporting_pop),
      ppp = NA,
      cpi = NA,
      pop_data_level = "national",
      gdp_data_level = "national",
      pce_data_level = "national",
      cpi_data_level = "national",
      ppp_data_level = "national",
      reporting_level     = "national",
      distribution_type = unique(distribution_type),
      gd_type = unique(gd_type),
      is_interpolated = FALSE,
      is_used_for_aggregation = TRUE
    ),
    by = .(survey_id, cache_id)
    ]

  dt <- rbind(dt_agg, dt)

  # Sort rows
  data.table::setorder(dt, survey_id)
}
