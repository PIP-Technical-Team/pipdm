#' @import data.table
#' @importFrom magrittr %>%
NULL

# Constants
df_gdp <- data.frame(
  country_code = rep('ABC', 3), year = as.numeric(2005:2007),
  gdp_data_level = rep(2, 3), gdp_domain = rep(1, 3),
  gdp = c(800, 802, 803))
df_pce <- data.frame(
  country_code = rep('ABC', 3), year = as.numeric(2005:2007),
  pce_data_level = rep(2, 3), pce_domain = rep(1, 3),
  pce = c(1000, 1005, 1007))
df_svy_anchor <- data.frame(
  wb_region_code = 'ECA', country_code = 'ABC',
  survey_name = 'SURV-123', survey_coverage = 'National',
  surveyid_year = c(2005, 2005, 2006), data_type = c('I', 'I', 'C'),
  survey_year = c(2005.5, 2005.0, 2006.3))

# db_create_nac_table
test_that('db_create_nac_table() works as expected', {
  # Create table to test against
  df <- data.table::data.table(
    country_code = rep('ABC', 3), year = as.numeric(2005:2007),
    data_level = rep('National', 3), domain = rep(1, 3),
    gdp = c(800, 802, 803), pce = c(1000, 1005, 1007))
  # Run pipeline
  out <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df, out)
})

# db_subset_nac_table
test_that('db_subset_nac_table() works as expected', {
  # Create table to test against
  df <- data.table::data.table(
    country_code = 'ABC', year = 2005, data_level = 'National',
    domain = 1, gdp = 800, pce = 1000)
  # Run pipeline
  out <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level)) %>%
    db_subset_nac_table(pip_years = 2005)
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df, out)
})

# db_merge_anchor_nac
test_that('db_merge_anchor_nac() works as expected', {
  # Create tables to test against
  df <- data.table::data.table(
    country_code = 'ABC', survey_coverage = 'National',
    wb_region_code = 'ECA', survey_year = c(2005.5, 2005.0, 2006.3),
    data_type = c('I', 'I', 'C'), svy_year_mean =  c(100, 100, 98),
    domain = 1)
  df_data <- data.table::data.table(
    year = as.numeric(2005:2007),
    gdp = c(800, 802, 803),
    pce = c(1000, 1005, 1007))
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  out <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor)
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df[,1:7], out[,1:7])
  expect_equal(df_data$year, out$data[[1]]$year)
  expect_equal(df_data$gdp, out$data[[1]]$gdp)
  expect_equal(df_data$pce, out$data[[1]]$pce)
})

# db_adjust_nac_values
test_that('db_adjust_nac_values() works as expected', {
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  out <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor) %>%
    db_adjust_nac_values()
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(out$adjusted_svy_gdp, c(801.0, 800.0, 802.3))
  expect_equal(out$adjusted_svy_pce, c(1002.5, 1000.0, 1005.6))
})

# db_create_lkup_table
test_that('db_create_lkup_table() works as expected', {
  # Create tables to test against
  df <- data.table::data.table(
    country_code_index  = 'ABC',
    survey_coverage_index = 'National',
    data_type_index = c(rep('C', 3), rep('I', 3)),
    domain_index = 1,
    reference_year_index = rep(2005:2007, times = 2),
    gdp = rep(c(800, 802, 803), times = 2),
    pce = rep(c(1000, 1005, 1007), times = 2)) %>%
    data.table::setkey()
  df_data <- data.table::data.table(
    country_code = 'ABC', survey_coverage = 'National',
    wb_region_code = 'ECA', survey_year = 2006.3,
    data_type = 'C', svy_year_mean = 98,
    domain = 1, adjusted_svy_gdp = 802.3,
    adjusted_svy_pce = 1005.6, reference_year =  2005)
  df_data2 <- data.table::data.table(
    country_code = 'ABC', survey_coverage = 'National',
    wb_region_code = 'ECA', survey_year = c(2005.0, 2005.5),
    data_type = 'I', svy_year_mean = 100,
    domain = 1, adjusted_svy_gdp = c(800, 801),
    adjusted_svy_pce = c(1000, 1002.5), reference_year = 2007)
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  df_svy <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor) %>%
    db_adjust_nac_values()
  out <- db_create_lkup_table(df_svy, df_nac, ref_years = 2005:2007)
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df[,1:7], out[,1:7])
  expect_equal(df_data$survey_year, out$data[[1]]$survey_year)
  expect_equal(df_data$data_type, out$data[[1]]$data_type)
  expect_equal(df_data$svy_year_mean, out$data[[1]]$svy_year_mean)
  expect_equal(df_data$adjusted_svy_gdp, out$data[[1]]$adjusted_svy_gdp)
  expect_equal(df_data$adjusted_svy_pce, out$data[[1]]$adjusted_svy_pce)
  expect_equal(df_data$reference_year, out$data[[1]]$reference_year)
  expect_equal(df_data2$survey_year, out$data[[6]]$survey_year)
  expect_equal(df_data2$data_type, out$data[[6]]$data_type)
  expect_equal(df_data2$svy_year_mean, out$data[[6]]$svy_year_mean)
  expect_equal(df_data2$adjusted_svy_gdp, out$data[[6]]$adjusted_svy_gdp)
  expect_equal(df_data2$adjusted_svy_pce, out$data[[6]]$adjusted_svy_pce)
  expect_equal(df_data2$reference_year, out$data[[6]]$reference_year)
})

# db_get_closest_surveys
test_that('db_get_closest_surveys() works as expected', {
  # Create tables to test against
  df <- data.table::data.table(
    country_code_index  = 'ABC',
    survey_coverage_index = 'National',
    data_type_index = c(rep('C', 3), rep('I', 3)),
    domain_index = 1,
    reference_year_index = rep(2005:2007, times = 2),
    gdp = rep(c(800, 802, 803), times = 2),
    pce = rep(c(1000, 1005, 1007), times = 2)) %>%
    data.table::setkey()
  df_data <- data.table::data.table(
    country_code = 'ABC', survey_coverage = 'National',
    wb_region_code = 'ECA', survey_year = 2006.3,
    data_type = 'C', svy_year_mean = 98,
    domain = 1, adjusted_svy_gdp = 802.3,
    adjusted_svy_pce = 1005.6, reference_year =  2005)
  df_data2 <- data.table::data.table(
    country_code = 'ABC', survey_coverage = 'National',
    wb_region_code = 'ECA', survey_year = 2005.5,
    data_type = 'I', svy_year_mean = 100,
    domain = 1, adjusted_svy_gdp = 801,
    adjusted_svy_pce = 1002.5, reference_year =  2007)
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  df_svy <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor) %>%
    db_adjust_nac_values()
  out <- db_create_lkup_table(df_svy, df_nac, ref_years = 2005:2007) %>%
    db_get_closest_surveys()
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df[,1:7], out[,1:7])
  expect_equal(df_data$survey_year, out$svy_items[[1]]$survey_year)
  expect_equal(df_data$data_type, out$svy_items[[1]]$data_type)
  expect_equal(df_data$svy_year_mean, out$svy_items[[1]]$svy_year_mean)
  expect_equal(df_data$adjusted_svy_gdp, out$svy_items[[1]]$adjusted_svy_gdp)
  expect_equal(df_data$adjusted_svy_pce, out$svy_items[[1]]$adjusted_svy_pce)
  expect_equal(df_data$reference_year, out$svy_items[[1]]$reference_year)
  expect_equal(df_data2$survey_year, out$svy_items[[6]]$survey_year)
  expect_equal(df_data2$data_type, out$svy_items[[6]]$data_type)
  expect_equal(df_data2$svy_year_mean, out$svy_items[[6]]$svy_year_mean)
  expect_equal(df_data2$adjusted_svy_gdp, out$svy_items[[6]]$adjusted_svy_gdp)
  expect_equal(df_data2$adjusted_svy_pce, out$svy_items[[6]]$adjusted_svy_pce)
  expect_equal(df_data2$reference_year, out$svy_items[[6]]$reference_year)
})

# db_adjust_welfare_mean
test_that('db_adjust_welfare_mean() works as expected', {
  # Create table to test against
  df <- data.table::data.table(
    country_code  = 'ABC',
    survey_coverage = 'National',
    data_type = c(rep('C', 3), rep('I', 3)),
    domain = 1,
    reference_year = rep(2005:2007, times = 2),
    gdp = rep(c(800, 802, 803), times = 2),
    pce = rep(c(1000, 1005, 1007), times = 2),
    adjustment_method =
      c(rep('one_point_adjusted_with_pce', 3),
        'one_point_same_as_survey_year',
        rep('one_point_adjusted_with_pce', 2)),
    survey_year = c(rep(2006.3, 3), 2005.0, rep(2005.5, 2)),
    ref_year_mean = c(97.45426, 97.94153, 98.13644,
                      100, 100.24938, 100.44888),
    svy_year_mean = c(rep(98, 3), rep(100, 3)),
    wb_region_code = 'ECA') %>%
    data.table::setkey(
      country_code, survey_coverage, data_type,
      domain, reference_year, gdp, pce)
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  df_svy <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor) %>%
    db_adjust_nac_values()
  out <- db_create_lkup_table(df_svy, df_nac, ref_years = 2005:2007) %>%
    db_get_closest_surveys() %>%
    db_adjust_welfare_mean()
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(out, df, tolerance = 1.5e-7)
})

# db_select_lineup_surveys
test_that('db_select_lineup_surveys() works as expected', {
  # Create table to test against
  df <- data.table::data.table(
    country_code  = 'ABC',
    survey_coverage = 'National',
    data_type = c('I', rep('C', 2)),
    domain = 1,
    reference_year = 2005:2007,
    gdp = c(800, 802, 803),
    pce = c(1000, 1005, 1007),
    adjustment_method = c(
      'one_point_same_as_survey_year',
      rep('one_point_adjusted_with_pce', 2)),
    survey_year = c(2005.0, rep(2006.3, 2)),
    ref_year_mean = c(100, 97.94153, 98.13644),
    svy_year_mean = c(100, rep(98, 2)),
    wb_region_code = 'ECA') %>%
    data.table::setkey(country_code, survey_coverage,
                       reference_year)
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  df_svy <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor) %>%
    db_adjust_nac_values()
  out <- db_create_lkup_table(df_svy, df_nac, ref_years = 2005:2007) %>%
    db_get_closest_surveys() %>%
    db_adjust_welfare_mean() %>%
    db_select_lineup_surveys()
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df, out, ignore.col.order = TRUE, tolerance = 1.5e-7)
})

# db_finalize_ref_year_table
test_that('db_finalize_ref_year_table() works as expected', {
  # Create table to test against
  df <- data.table::data.table(
    country_code  = 'ABC',
    survey_coverage = 'National',
    data_type = c('I', rep('C', 2)),
    domain = 1,
    reference_year = 2005:2007,
    adjustment_method = c(
      'one_point_same_as_survey_year',
      rep('one_point_adjusted_with_pce', 2)),
    survey_year = c(2005.0, rep(2006.3, 2)),
    ref_year_mean = c(100, 97.94153, 98.13644),
    svy_year_mean = c(100, rep(98, 2)),
    wb_region_code = 'ECA') %>%
    data.table::setkey(country_code, survey_coverage,
                       reference_year)
  # Run pipeline
  df_nac <- db_create_nac_table(df_gdp, df_pce) %>%
    transform(data_level = recode_data_level(data_level))
  df_svy_anchor$svy_year_mean <- c(100, 100, 98) # This is temporary,  'svy_year_mean' should come from the svy_mean table
  df_svy <- db_merge_anchor_nac(nac_table = df_nac, svy_anchor = df_svy_anchor) %>%
    db_adjust_nac_values()
  out <- db_create_lkup_table(df_svy, df_nac, ref_years = 2005:2007) %>%
    db_get_closest_surveys() %>%
    db_adjust_welfare_mean() %>%
    db_select_lineup_surveys() %>%
    db_finalize_ref_year_table(df_svy_anchor)
  # Test equality
  expect_identical(class(out), c('data.table', 'data.frame'))
  expect_equal(df, out, ignore.col.order = TRUE, tolerance = 1.5e-7)
})



