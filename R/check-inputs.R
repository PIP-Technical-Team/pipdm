#' Check input columns
#'
#' `check_inputs_columns()` checks if a table has the correct columns. It fails
#' if there are any column names that don't belong in the table *or* if there
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
  col_types <- c('character', 'numeric', 'character', 'numeric', 'character')
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
  col_types <- c('character', 'numeric', 'character', 'numeric', 'character')
  check_inputs_columns(pce_table, cols, 'pce_table')
  pce_table <- pce_table[cols]
  check_inputs_column_types(pce_table, cols, col_types, 'pce_table')
}

#' check_inputs_pop_table
#' @param pop_table data.frame: The Population table from pipaux.
#' @return logical
#' @noRd
check_inputs_pop_table <- function(pop_table) {
  cols <- c('country_code', 'year', 'pop_data_level', 'pop', 'pop_domain')
  col_types <- c('character', 'numeric', 'character', 'numeric', 'character')
  check_inputs_columns(pop_table, cols, 'pop_table')
  pop_table <- pop_table[cols]
  check_inputs_column_types(pop_table, cols, col_types, 'pop_table')
}

