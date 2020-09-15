#' @importFrom dplyr recode
#' @importFrom rlang abort
NULL

#' Recode data_level and survey coverage
#'
#' Recodes 'data_level' variables in the auxiliary data to match
#' the 'survey_coverage' variable in the Survey Anchor table.
#'
#' AE: Note this merge should really be done on an other variable in
#' the Survey Anchor table.
#' @noRd
recode_data_level <- function(x){
  dplyr::recode(x, '0' = 'Rural', '1' = 'Urban', '2' = 'National')
}
recode_survey_coverage <- function(x){
  dplyr::recode(x, N = 'National', U = 'Urban', R = 'Rural')
}

#' Find countries without national coverage
#'
#' Check the survey anchor table for countries without any national survey
#' coverage. If any such countries exists the functions returns a character
#' vector with country codes for those countries.
#'
#' @param svy_anchor data.frame: A table with survey metadata information.
#'
#' @seealso `db_merge_anchor_na()` `db_finalize_ref_year_table()`
#' @noRd
check_no_national_survey <- function(svy_anchor){
  tmp <- table(svy_anchor$country_code, svy_anchor$survey_coverage)
  tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
  names(tmp) <- c('country_code', 'survey_coverage', 'freq')
  cc <- tmp[(tmp$survey_coverage == 'National' & tmp$freq == 0), ][['country_code']]
  return(cc)
}

#' Find countries with only one coverage level
#'
#' Check the survey anchor table for countries that only have unique coverage
#' level. This is typically used together with the output from
#' `check_no_national_survey()`. Returns a data frame with the country code
#' and coverage level.
#'
#' @param cc character: A vector with country codes. Output of
#'   \code{check_no_national_survey}.
#' @param svy_anchor data.frame: A table with survey metadata information.
#'
#' @seealso `db_finalize_ref_year_table()`
#' @noRd
find_unique_coverage <- function(cc, svy_anchor){
  x <- unique(svy_anchor[svy_anchor$country_code == cc,]$survey_coverage)
  if (length(x) == 1) {
    out <- data.frame(country_code = cc, survey_coverage = x)
    return(out)
  }
}

#' Check input columns
#'
#' `check_inputs_columns()` checks if a table has the correct columns. It fails
#' if there are any column names that don't belong in the table _or_ if there
#' any columns that should be in the table but aren't found. If any errors are
#' found the function returns a bullet list with all errors.
#'
#' @param df data.frame: A table to check.
#' @param cols character: A vector with correct column names.
#' @param table_name character: The name of the table.
#'
#' @noRd
check_inputs_columns <- function(df, cols, table_name){

  msg <- sprintf('`%s` doesn\'t have the correct columns:', table_name)
  nms <- names(df)

  if (!all(nms %in% cols) | !all(cols %in% nms)) {
    # Columns that don't belong
    non_valid_cols <- nms[!nms %in% cols]
    # Columns that must be in the table
    missing_cols <- cols[!cols %in% nms]
    # Create a bullet list vector
    n_error <- length(non_valid_cols) + length(missing_cols)
    tmp <- rep(NA, n_error)
    for (i in seq_along(n_error)) {
      tmp[i] <- sprintf('`%s` is not a valid column.', non_valid_cols[i])
    }
    for (i in seq_along(missing_cols)) {
      tmp[i + length(non_valid_cols)] <- sprintf('`%s` is missing.' , missing_cols[i])
    }
    # Add 'x' as prefix
    names(tmp) <- rep('x', n_error)
    # Stop message
    rlang::abort(c(msg, tmp))
  }
}

#' Check input column types
#'
#' `check_inputs_column_types()` checks if a table has the correct column types.
#' If any errors are found the function returns a bullet list with all errors.
#'
#' @param df data.frame: A table to check.
#' @param cols character: A vector with correct column names.
#' @param col_types character: A vector with correct column types.
#' @param table_name character: The name of the table.
#'
#' @noRd
check_inputs_column_types <- function(df, cols, col_types, table_name){

  msg <- sprintf('`%s` doesn\'t have the correct column types:', table_name)
  check <- vapply(df, class, FUN.VALUE = character(1), USE.NAMES = FALSE)

  if (!identical(col_types, check)) {
    # Create a bullet list vector
    msg2 <- '`%s` must be a %s vector; not %s.'
    n_error <- sum(col_types != check)
    which_error <- which(col_types != check)
    tmp <- rep(NA, n_error)
    for (i in seq_along(which_error)) {
      tmp[i] <- sprintf(msg2, cols[which_error][i], col_types[which_error][i], check[which_error][i])
    }
    # Add 'x' as prefix
    names(tmp) <- rep('x', n_error)
    # Stop message
    rlang::abort(c(msg, tmp))
  }
}

#' check_inputs_gdp_table
#' @param gdp_table data.frame: The GDP table from pipaux.
#' @return logical
#' @noRd
check_inputs_gdp_table <- function(gdp_table) {
  cols <- c('country_code', 'year', 'gdp_data_level', 'gdp', 'gdp_domain')
  col_types <- c('character', 'numeric', 'numeric', 'numeric', 'numeric')
  check_inputs_columns(gdp_table, cols, 'gdp_table')
  gdp_table <- gdp_table[cols]
  check_inputs_column_types(gdp_table, cols, col_types, 'gdp_table')
}

#' check_inputs_pce_table
#' @param pce_table data.frame: The PCE table from pipaux.
#' @return logical
#' @noRd
check_inputs_pce_table <- function(pce_table) {
  cols <- c('country_code', 'year', 'pce_data_level', 'pce', 'pce_domain')
  col_types <- c('character', 'numeric', 'numeric', 'numeric', 'numeric')
  check_inputs_columns(pce_table, cols, 'pce_table')
  pce_table <- pce_table[cols]
  check_inputs_column_types(pce_table, cols, col_types, 'pce_table')
}

#' check_inputs_nac_table
#' @param nac_table data.frame: A combined GDP and PCE table.
#' @return logical
#' @noRd
check_inputs_nac_table <- function(nac_table) {
  cols <- c('country_code', 'year', 'data_level', 'domain', 'pce', 'gdp')
  col_types <- c('character', 'numeric', 'character', 'numeric', 'numeric', 'numeric')
  check_inputs_columns(nac_table, cols, 'nac_table')
  nac_table <- nac_table[, cols, with = FALSE]
  check_inputs_column_types(nac_table, cols, col_types, 'nac_table')
}

#' check_inputs_pop_table
#' @param pop_table data.frame: The Population table from pipaux.
#' @return logical
#' @noRd
check_inputs_pop_table <- function(pop_table) {
  cols <- c('country_code', 'year', 'pop_data_level', 'pop', 'pop_domain')
  col_types <- c('character', 'numeric', 'numeric', 'numeric', 'numeric')
  check_inputs_columns(pop_table, cols, 'pop_table')
  pop_table <- pop_table[cols]
  check_inputs_column_types(pop_table, cols, col_types, 'pop_table')
}

#' check_inputs_ppp_table
#' @param ppp_table data.frame: The PPP table from pipaux.
#' @return logical
#' @noRd
check_inputs_ppp_table <- function(ppp_table) {
  cols <- c('country_code', 'ppp_year', 'release_version', 'adaptation_version', 'ppp',
            'ppp_default', 'ppp_default_by_year', 'ppp_domain', 'ppp_data_level')
  col_types <- c('character', 'character', 'character', 'character', 'numeric',
                 'logical', 'logical', 'numeric', 'character')
  check_inputs_columns(ppp_table, cols, 'ppp_table')
  ppp_table <- ppp_table[cols]
  check_inputs_column_types(ppp_table, cols, col_types, 'ppp_table')
}

#' check_inputs_cpi_table
#' @param cpi_table data.frame: The CPI table from pipaux.
#' @return logical
#' @noRd
check_inputs_cpi_table <- function(cpi_table) {
  cols <- c('country_code', 'surveyid_year', 'survey_year', 'cpi', 'ccf',
            'cpi_domain', 'cpi_data_level')
  col_types <- c('character', 'numeric', 'numeric', 'numeric', 'numeric',
                 'numeric', 'numeric')
  check_inputs_columns(cpi_table, cols, 'cpi_table')
  check_inputs_column_types(cpi_table, cols, col_types, 'cpi_table')
}

#' check_inputs_svy_anchor
#' @param svy_anchor data.frame: The Survey Anchor table from pipaux.
#' @return logical
#' @noRd
check_inputs_svy_anchor <- function(svy_anchor) {
  cols <- c('wb_region_code','country_code', 'survey_name', 'surveyid_year', 'survey_coverage',
            'data_type', 'survey_year',
            'svy_year_mean') # 'svy_year_mean' should come from the svy_mean table
  col_types <- c('character', 'character', 'character', 'numeric', 'character',
                 'character', 'numeric',
                 'numeric') # 'svy_year_mean' should come from the svy_mean table
  check_inputs_columns(svy_anchor, cols, 'svy_anchor')
  svy_anchor <- svy_anchor[cols]
  check_inputs_column_types(svy_anchor, cols, col_types, 'svy_anchor')
}

#' check_inputs_ref_years
#' @noRd
check_inputs_ref_years <- function(x){
  if (!is.numeric(x))
    rlang::abort(c('`ref_years` must be a numeric or integer vector: ',
                   sprintf('You\'ve supplied a %s vector.', class(x))))
}

#' check_inputs_ref_years
#' @noRd
check_inputs_pip_years <- function(x){
  if (!is.numeric(x))
    rlang::abort(c('`pip_years` must be a numeric or integer vector: ',
                   sprintf('You\'ve supplied a %s vector.', class(x))))
}

#' check_inputs_db_class
#' @noRd
check_inputs_db_class <- function(df){
  if (!any(class(df) %in% 'data.table')) rlang::abort('`df` must be of class `data.table`.')
}
