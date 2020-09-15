# Test general helper functions

# check_no_national_survey
test_that('check_no_national_survey() works as expected', {
  df <- data.frame(country_code = c('NOR', 'CHN', 'CHN'), survey_year = 2005,
                   survey_coverage = c('National', 'Urban', 'Rural'))
  expect_identical(check_no_national_survey(df), 'CHN')
})

# find_unique_coverage
test_that('find_unique_coverage() works as expected', {
  df <- data.frame(country_code = c('NOR', 'ARG'), survey_year = 2005,
                   survey_coverage = c('National', 'Urban'))
  expect_identical(find_unique_coverage('ARG', df),
                   data.frame(country_code = 'ARG', survey_coverage = 'Urban'))
})

# recode_data_level
test_that('recode_data_level() works as expected', {
  out <- recode_data_level(c('0', '1', '2'))
  expect_identical(out, c('Rural', 'Urban', 'National'))
  out <- recode_data_level(0:2)
  expect_identical(out, c('Rural', 'Urban', 'National'))
})

# recode_survey_coverage
test_that('recode_survey_coverage() works as expected', {
  out <- recode_survey_coverage(c('R', 'U', 'N'))
  expect_identical(out, c('Rural', 'Urban', 'National'))
})

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
  df <- data.frame(country_code = 'ABC', year = 2005, gdp_data_level = 1,
                   gdp_domain = 1, gdp = 1000)
  expect_null(check_inputs_gdp_table(df))
  # Invalid column
  df <- data.frame(country_code = 'ABC', year = '2005', gdp_data_level = 1,
                   gdp_domain = 1, gdp = 1000, test = NA)
  expect_error(check_inputs_gdp_table(df),
               paste0('`gdp_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(country_code = 'ABC', year = '2005', gdp_data_level = 1,
                   gdp_domain = 1)
  expect_error(check_inputs_gdp_table(df),
               paste0('`gdp_table` doesn\'t have the correct columns:\n.*',
                      '`gdp` is missing.'))
  # Incorrect column type
  df <- data.frame(country_code = 'ABC', year = '2005', gdp_data_level = 1,
                   gdp_domain = 1, gdp = 1000)
  expect_error(check_inputs_gdp_table(df),
               paste0('`gdp_table` doesn\'t have the correct column types:\n.*',
                      '`year` must be a numeric vector; not character.'))
})

# check_inputs_pce_table
test_that('check_inputs_pce_table() works as expected', {
  # Correct table
  df <- data.frame(country_code = 'ABC', year = 2005, pce_data_level = 1,
                   pce_domain = 1, pce = 1000)
  expect_null(check_inputs_pce_table(df))
  # Invalid column
  df <- data.frame(country_code = 'ABC', year = '2005', pce_data_level = 1,
                   pce_domain = 1, pce = 1000, test = NA)
  expect_error(check_inputs_pce_table(df),
               paste0('`pce_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(country_code = 'ABC', year = '2005', pce_data_level = 1,
                   pce_domain = 1)
  expect_error(check_inputs_pce_table(df),
               paste0('`pce_table` doesn\'t have the correct columns:\n.*',
                      '`pce` is missing.'))
  # Incorrect column type
  df <- data.frame(country_code = 'ABC', year = '2005', pce_data_level = 1,
                   pce_domain = 1, pce = 1000)
  expect_error(check_inputs_pce_table(df),
               paste0('`pce_table` doesn\'t have the correct column types:\n.*',
                      '`year` must be a numeric vector; not character.'))
})

# check_inputs_nac_table
test_that('check_inputs_nac_table() works as expected', {
  # Correct table
  df <- data.table::data.table(
    country_code = 'ABC', year = 2005, data_level = 'National',
    domain = 1, pce = 1000, gdp = 800)
  expect_null(check_inputs_nac_table(df))
  # Invalid column
  df <- data.table::data.table(
    country_code = 'ABC', year = 2005, data_level = 'National',
    domain = 1, pce = 1000, gdp = 800, test = NA)
  expect_error(check_inputs_nac_table(df),
               paste0('`nac_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.table::data.table(
    country_code = 'ABC', year = 2005, data_level = 'National',
    domain = 1, gdp = 800)
  expect_error(check_inputs_nac_table(df),
               paste0('`nac_table` doesn\'t have the correct columns:\n.*',
                      '`pce` is missing.'))
  # Incorrect column type
  df <- data.table::data.table(
    country_code = 'ABC', year = '2005', data_level = 'National',
    domain = 1, pce = 1000, gdp = 800)
  expect_error(check_inputs_nac_table(df),
               paste0('`nac_table` doesn\'t have the correct column types:\n.*',
                      '`year` must be a numeric vector; not character.'))
})

# check_inputs_ppp_table
test_that('check_inputs_ppp_table() works as expected', {
  # Correct table
  df <- data.frame(country_code = 'ABC', ppp_year = '2005', release_version = '1',
                   adaptation_version = '1', ppp = 1000, ppp_default = TRUE,
                   ppp_default_by_year = FALSE, ppp_domain = 1,
                   ppp_data_level = '2')
  expect_null(check_inputs_ppp_table(df))
  # Invalid column
  df <- data.frame(country_code = 'ABC', ppp_year = '2005', release_version = '1',
                   adaptation_version = '1', ppp = 1000, ppp_default = TRUE,
                   ppp_default_by_year = FALSE, ppp_domain = 1,
                   ppp_data_level = '2', test = NA)
  expect_error(check_inputs_ppp_table(df),
               paste0('`ppp_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(country_code = 'ABC', ppp_year = '2005', release_version = '1',
                   adaptation_version = '1', ppp_default = TRUE,
                   ppp_default_by_year = FALSE, ppp_domain = 1,
                   ppp_data_level = '2')
  expect_error(check_inputs_ppp_table(df),
               paste0('`ppp_table` doesn\'t have the correct columns:\n.*',
                      '`ppp` is missing.'))
  # Incorrect column type
  df <- data.frame(country_code = 'ABC', ppp_year = 2005, release_version = '1',
                   adaptation_version = '1', ppp = 1000, ppp_default = TRUE,
                   ppp_default_by_year = FALSE, ppp_domain = 1,
                   ppp_data_level = '2')
  expect_error(check_inputs_ppp_table(df),
               paste0('`ppp_table` doesn\'t have the correct column types:\n.*',
                      '`ppp_year` must be a character vector; not numeric.'))
})

# check_inputs_cpi_table
test_that('check_inputs_cpi_table() works as expected', {
  # Correct table
  df <- data.frame(country_code = 'ABC', surveyid_year = 2005, survey_year = 1,
                   cpi = 2.3, ccf = 1, cpi_domain = 1, cpi_data_level = 1)
  expect_null(check_inputs_cpi_table(df))
  # Invalid column
  df <- data.frame(country_code = 'ABC', surveyid_year = 2005, survey_year = 1,
                   cpi = 2.3, ccf = 1, cpi_domain = 1, cpi_data_level = 1,
                   test = NA)
  expect_error(check_inputs_cpi_table(df),
               paste0('`cpi_table` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(country_code = 'ABC', surveyid_year = 2005, survey_year = 1,
                   ccf = 1, cpi_domain = 1, cpi_data_level = 1)
  expect_error(check_inputs_cpi_table(df),
               paste0('`cpi_table` doesn\'t have the correct columns:\n.*',
                      '`cpi` is missing.'))
  # Incorrect column type
  df <- data.frame(country_code = 'ABC', surveyid_year = '2005', survey_year = 1,
                   cpi = 2.3, ccf = 1, cpi_domain = 1, cpi_data_level = 1)
  expect_error(check_inputs_cpi_table(df),
               paste0('`cpi_table` doesn\'t have the correct column types:\n.*',
                      '`surveyid_year` must be a numeric vector; not character'))
})

# check_inputs_svy_anchor
test_that('check_inputs_svy_anchor() works as expected', {
  # Correct table
  df <- data.frame(wb_region_code = 'ECA', country_code = 'ABC', survey_name = 'SURV-123',
                   surveyid_year = 2005, survey_coverage = 'National', svy_year_mean = 1, # Tmp
                   data_type = 'I', survey_year = 2005.5)
  expect_null(check_inputs_svy_anchor(df))
  # Invalid column
  df <- data.frame(wb_region_code = 'ECA', country_code = 'ABC', survey_name = 'SURV-123',
                   surveyid_year = 2005, survey_coverage = 'National', svy_year_mean = 1, # Tmp
                   data_type = 'I', survey_year = 2005.5, test = NA)
  expect_error(check_inputs_svy_anchor(df),
               paste0('`svy_anchor` doesn\'t have the correct columns:\n.*',
                      '`test` is not a valid column.'))
  # Missing column
  df <- data.frame(wb_region_code = 'ECA', country_code = 'ABC', survey_name = 'SURV-123',
                   surveyid_year = 2005, data_type = 'I', survey_year = 2005.5,
                   svy_year_mean = 1) # Tmp
  expect_error(check_inputs_svy_anchor(df),
               paste0('`svy_anchor` doesn\'t have the correct columns:\n.*',
                      '`survey_coverage` is missing.'))
  # Incorrect column type
  df <- data.frame(wb_region_code = 'ECA', country_code = 'ABC', survey_name = 'SURV-123',
                   surveyid_year = 2005, survey_coverage = 'National', svy_year_mean = 1, # Tmp
                   data_type = 'I', survey_year = '2005.5')
  expect_error(check_inputs_svy_anchor(df),
               paste0('`svy_anchor` doesn\'t have the correct column types:\n.*',
                      '`survey_year` must be a numeric vector; not character'))
})
