#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @importFrom purrr map map2 map2_dbl pmap pmap_chr is_empty
#' @importFrom tidyr expand_grid
#' @importFrom tidyfast dt_nest
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('country_code_index','country_code', 'survey_coverage', 'survey_coverage_index',
      'reference_year', 'reference_year_index', 'domain', 'domain_index',
      'data_type', 'data_type_index', 'data_level', 'gdp', 'pce',
      'ref_adj_data', 'svy_lineup_data')
  )

#' Create reference year table
#'
#' Create a table with adjusted welfare means for each reference year.
#'
#' `db_create_ref_year_table()` creates a table for with interpolated or
#' extrapolated welfare means for all specified reference years based on the
#' provided survey, GDP and PCE data.
#'
#' @param gdp_table data.frame: A table with GDP data.
#' @param pce_table data.frame: A table with PCE data.
#' @param svy_anchor data.frame: A table with survey metadata information.
#' @param svy_mean_table data.frame: A table with survey means.
#' @param ref_years numeric: a vector with reference years.
#' @param pip_years numeric: A vector with calender years used in PIP.
#'
#' @seealso [adjust_welfare_mean()]
#' @return `data.frame`
#' @export
db_create_ref_year_table <- function(gdp_table,
                                     pce_table,
                                     svy_anchor,
                                     svy_mean_table = NULL, # TBD
                                     ref_years = pipdm:::reference_years,
                                     pip_years = pipdm:::pip_years) {

  # Create a combined table with National Accounts data
  df_nac <-
    db_create_nac_table(gdp_table, pce_table) %>%
    db_subset_nac_table(pip_years = pip_years) %>%
    transform(data_level = recode_data_level(data_level)) # Move to pipaux?

  # Create a table with survey metadata information and adjusted values
  # for GDP and PCE for surveys that span multiple years
  df_syv <-
    db_merge_anchor_nac(svy_anchor, df_nac) %>%
    db_adjust_nac_values()

  # TBC: Add survey mean from separate table?

  # Create reference year table
  df_ref <-
    db_create_lkup_table(df_syv, df_nac, ref_years) %>%
    db_get_closest_surveys() %>%
    db_adjust_welfare_mean() %>%
    db_select_lineup_surveys() %>%
    db_finalize_ref_year_table(svy_anchor) %>%
    as.data.frame()

  return(df_ref)

}

#' Merge GDP and PCE data
#'
#' Create a table with national accounts (NAC) data by merging separate tables
#' for GDP and PCE.
#'
#' Both input tables should contain ISO-3 country codes, calender years and
#' information indicating the data level and domain (e.g national, urban/rural
#' or sub-national) of the variable in question.
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

#' Merge survey anchor with national accounts data
#'
#' Create a combined table of survey metadata information and national accounts
#' data.
#'
#' @param svy_anchor data.frame: A table with survey metadata information.
#' @param nac_table data.table: Output of [db_subset_nac_table()].
#'
#' @return `data.table`
#' @keywords internal
db_merge_anchor_nac <- function(svy_anchor, nac_table){

  # Select relevant survey anchor columns
  sa_vars <- c('wb_region_code', 'country_code', 'survey_year', 'survey_coverage',
               'data_type', 'svy_year_mean')
  svy_anchor <- svy_anchor[sa_vars]

  # Recode survey coverage. Move to pipaux?
  svy_anchor <- svy_anchor %>%
    transform(survey_coverage = recode_survey_coverage(survey_coverage))

  # Check for countries without any national surveys
  cc <- check_no_national_survey(svy_anchor)

  # Add national coverage rows for countries without national surveys
  if (!purrr::is_empty(cc)) {
    message('Note: National coverage rows have been added for \'',
            paste(cc, collapse = '\', \''), '\'.' )
    rows_to_add <-
      svy_anchor[svy_anchor$country_code %in% cc,] %>%
      transform(survey_coverage = 'National')
    svy_anchor <- rbind(svy_anchor, rows_to_add)
  }

  # Create nested NAC table
  nac_nested <- nac_table %>%
    tidyfast::dt_nest(country_code, data_level, domain, .key = 'data')

  # Merge nac_nested with svy_anchor (inner join)
  df <- merge(svy_anchor, nac_nested, all = FALSE,
              by.x = c('country_code', 'survey_coverage'),
              by.y = c('country_code', 'data_level'))

  return(df)
}

#' Adjust national account values
#'
#' Adjust GDP and PCE values for surveys that span multiple calender
#' years.
#'
#' Values are adjusted by the weighted average of the years in question.
#'
#' @param df data.table: Output of [db_merge_anchor_nac()].
#'
#' @seealso [adjust_aux_values()]
#' @return `data.table`
#' @keywords internal
db_adjust_nac_values <- function(df){

  # Adjust GDP and PCE values for surveys spanning two calender years
  df$adjusted_svy_gdp <- purrr::map2_dbl(df$survey_year, df$data, adjust_aux_values, value_var = 'gdp')
  df$adjusted_svy_pce <- purrr::map2_dbl(df$survey_year, df$data, adjust_aux_values, value_var = 'pce')

  # Remove nested data column
  df$data <- NULL

  return(df)

}

#' Create look-up table
#'
#' Create a look-up table with information on all reference years and surveys
#' for further data manipulation.
#'
#' @param df data.table: Output of [db_adjust_nac_values()].
#' @param nac_table data.table: Output of [db_subset_nac_table()].
#' @param ref_years numeric: Vector with reference years.
#'
#' @return `data.table`
#' @keywords internal
db_create_lkup_table <- function(df, nac_table, ref_years) {

  # Add reference year column
  df <- df %>% tidyr::expand_grid(reference_year = ref_years)

  # Add index columns
  df <- df %>%
    transform(country_code_index = country_code,
              survey_coverage_index = survey_coverage,
              data_type_index = data_type,
              domain_index = domain,
              reference_year_index = reference_year)

  # Merge with GDP and PCE data (left join)
  df <- merge(df, nac_table, all.x = TRUE,
              by.x = c('country_code', 'survey_coverage_index',
                       'domain_index', 'reference_year_index'),
              by.y = c('country_code', 'data_level', 'domain', 'year'))

  # Order by index columns
  df <- df[order(df$country_code_index, df$data_type_index, df$domain_index,
                 df$survey_coverage_index, df$reference_year_index),]

  # Nest by index columns
  df <- df %>%
    tidyfast::dt_nest(
      country_code_index, survey_coverage_index,
      data_type_index, domain_index,
      reference_year_index, gdp, pce,
      .key = 'data')

  # What to do when both GDP and PCE is missing? Remove?
  na_check <- is.na(df$gdp) & is.na(df$pce)
  if (any(na_check)) {
    message('Note: ', sum(na_check), ' country-year(s) are missing both GDP and PCE values. ',
            'These rows were removed.')
    df <- df[!(is.na(df$gdp) & is.na(df$pce))]
  }

  return(df)

}

#' Get closest surveys for all reference years
#'
#' Retrieve the closest surveys for each reference year.
#'
#' @param df data.table: Output of [db_create_lkup_table()].
#'
#' @seealso [get_closest_surveys()]
#' @return `data.table`
#' @keywords internal
db_get_closest_surveys <- function(df){

  # Create a nested list of survey line-up tables
  df$svy_items <- purrr::map2(df$data, df$reference_year_index, get_closest_surveys)

  # Remove nested 'data' column
  df$data <- NULL

  return(df)
}

#' Adjust the welfare mean for all reference years
#'
#' Adjust the welfare mean for all countries and reference years.
#'
#' @param df data.table: Output of [db_get_closest_surveys()].
#'
#' @seealso [get_welfare_adjustment_method()] [adjust_welfare_mean()]
#' @return `data.table`
#' @keywords internal
db_adjust_welfare_mean <- function(df){

  # Fetch adjustment method
  df$adjustment_method <-
    purrr::pmap_chr(list(svy_table = df$svy_items,
                         ref_year = df$reference_year_index,
                         ref_gdp = df$gdp, ref_pce = df$pce),
                get_welfare_adjustment_method)

  # Create a nested list of adjusted reference year mean tables
  df$ref_adj_data <-
    purrr::pmap(list(svy_table = df$svy_items,
                     ref_pce = df$pce, ref_gdp = df$gdp,
                     method = df$adjustment_method),
                safe_adjust_welfare_mean)
  df$ref_adj_data <- purrr::map(df$ref_adj_data , 'result')

  # Remove nested 'svy_items' column
  df$svy_items <- NULL

  # Remove '_index' from column names
  names(df) <- sub('[_]index$', '', names(df))

  # Unnest 'ref_adj_data' and combine with remaining columns
  df <- tidyfast::dt_unnest(df, ref_adj_data)

  return(df)
}

#' Select correct line-up surveys
#'
#' Select a single line-up survey when there are multiple line-up surveys
#' available.
#'
#' @param df data.table: Output of [db_adjust_welfare_mean()].
#'
#' @seealso [select_lineup_survey()]
#' @return `data.table`
#' @keywords internal
db_select_lineup_surveys <- function(df){

  # Define NULL values
  # country_code <- survey_coverage <- reference_year <- svy_lineup_data <- NULL

  # Nest by country, coverage and reference year
  df <- tidyfast::dt_nest(df, country_code, survey_coverage, reference_year, .key = 'data')

  # Fetch surveys to be used for line-up
  df$svy_lineup_data <- purrr::map2(df$data, df$reference_year, select_lineup_survey)

  # Remove nested 'data' column
  df$data <- NULL

  # Unnest 'svy_lineup_data' and combine with remaining columns
  df <- tidyfast::dt_unnest(df, svy_lineup_data)

  return(df)
}

#' Finalize reference year table
#'
#' Finalize the reference year table by subsetting the correct rows and
#' columns.
#'
#' @param df data.table: Output of [db_select_lineup_surveys()].
#' @param svy_anchor data.frame: A table with survey metadata information.
#'
#' @return `data.table`
#' @keywords internal
db_finalize_ref_year_table <- function(df, svy_anchor){

  # Check for countries without any national surveys
  cc <- check_no_national_survey(svy_anchor)

  # Recode survey coverage for countries without any national surveys
  # and only one coverage level (e.g. ARG)
  if (!purrr::is_empty(cc)) {
    tmp <- purrr::map_df(cc, find_unique_coverage, svy_anchor)
    df$survey_coverage <- ifelse(df$country_code %in% tmp$country_code,
                                 tmp$survey_coverage, df$survey_coverage)
    message('Note: Recoding survey coverage for \'',
            paste(tmp$country_code, collapse = '\', \''), '\'.')
  }

  # Remove rows with national survey coverage and missing reference year mean
  na_check <- df$survey_coverage == 'National' & is.na(df$ref_year_mean)
  if (any(na_check)) {
    cc <- df[na_check]$country_code %>% unique()
    df <- df[!na_check]
    message('Note: ', sum(na_check), ' country-year(s) with national survey coverage have ',
            'missing values for the reference year mean. Removing such rows for \'',
            paste(cc, collapse = '\', \''), '\'.')
  }

  # Select final columns
  cols <- c('wb_region_code', 'country_code', 'reference_year', 'survey_year',
            'survey_coverage', 'domain', 'data_type', 'svy_year_mean',
            'ref_year_mean', 'adjustment_method')
  df <- df[, cols, with = FALSE]

  # Rename columns?

  return(df)

}
