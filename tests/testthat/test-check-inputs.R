# check_inputs_columns
test_that('check_inputs_columns() works as expected', {
  df <- data.frame(X1  = 'ABC', X2 = 2005)
  cols <- c('X1')
  expect_error(check_inputs_columns(df, cols, 'df'),
               paste0('`df` doesn\'t have the correct columns:\n.*',
                      '`X2` is not a valid column.'))
  cols <- c('X1', 'X2', 'X3')
  expect_error(check_inputs_columns(df, cols, 'df'),
               paste0('`df` doesn\'t have the correct columns:\n.*',
                      '`X3` is missing.'))
  cols <- c('X1', 'X3')
  expect_error(check_inputs_columns(df, cols, 'df'),
               paste0('`df` doesn\'t have the correct columns:\n.*',
                      '`X2` is not a valid column.\n.*`X3` is missing.'))
})

# check_inputs_column_types
test_that('check_inputs_column_types() works as expected', {
  df <- data.frame(X1  = 'ABC', X2 = '2005')
  cols <- names(df)
  col_types <- c('character', 'numeric')
  expect_error(
    check_inputs_column_types(df, cols, col_types, 'df'),
    paste0('`df` doesn\'t have the correct column types:\n.*',
           '`X2` must be a numeric vector; not character.'))
  df <- data.frame(X1  = 'ABC', X2 = 2005, X3 = as.factor('I'))
  cols <- names(df)
  col_types <- c('factor', 'numeric', 'character')
  expect_error(
    check_inputs_column_types(df, cols, col_types, 'df'),
    paste0('`df` doesn\'t have the correct column types:\n.*',
           '`X1` must be a factor vector; not character.\n.*',
           '`X3` must be a character vector; not factor.'))
})

# check_inputs_gdp_table
test_that('check_inputs_gdp_table() works as expected', {
  # Correct table
  df <- data.frame(country_code = 'ABC', year = 2005, gdp_data_level = 'national',
                   gdp_domain = 'national', gdp = 1000)
  expect_null(check_inputs_gdp_table(df))
  # Invalid column
  df <- data.frame(country_code = 'ABC', year = '2005', gdp_data_level = 'national',
                   gdp_domain = 'national', gdp = 1000, test = NA)
  expect_error(check_inputs_gdp_table(df),
               paste0('`gdp_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(country_code = 'ABC', year = '2005', gdp_data_level = 'national',
                   gdp_domain = 1)
  expect_error(check_inputs_gdp_table(df),
               paste0('`gdp_table` doesn\'t have the correct columns:\n.*',
                      '`gdp` is missing.'))
  # Incorrect column type
  df <- data.frame(country_code = 'ABC', year = '2005', gdp_data_level = 'national',
                   gdp_domain = 'national', gdp = 1000)
  expect_error(check_inputs_gdp_table(df),
               paste0('`gdp_table` doesn\'t have the correct column types:\n.*',
                      '`year` must be a numeric vector; not character.'))
})

# check_inputs_pce_table
test_that('check_inputs_pce_table() works as expected', {
  # Correct table
  df <- data.frame(country_code = 'ABC', year = 2005, pce_data_level = 'national',
                   pce_domain = 'national', pce = 1000)
  expect_null(check_inputs_pce_table(df))
  # Invalid column
  df <- data.frame(country_code = 'ABC', year = '2005', pce_data_level = 'national',
                   pce_domain = 'national', pce = 1000, test = NA)
  expect_error(check_inputs_pce_table(df),
               paste0('`pce_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(country_code = 'ABC', year = '2005', pce_data_level = 'national',
                   pce_domain = 1)
  expect_error(check_inputs_pce_table(df),
               paste0('`pce_table` doesn\'t have the correct columns:\n.*',
                      '`pce` is missing.'))
  # Incorrect column type
  df <- data.frame(country_code = 'ABC', year = '2005', pce_data_level = 'national',
                   pce_domain = 'national', pce = 1000)
  expect_error(check_inputs_pce_table(df),
               paste0('`pce_table` doesn\'t have the correct column types:\n.*',
                      '`year` must be a numeric vector; not character.'))
})

