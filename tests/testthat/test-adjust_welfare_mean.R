# Test adjust_welfare_mean() and its underlying helper functions

test_that('missing_pce_a() works as expected', {
  out <- missing_pce_a(2005.5)
  expect_true(all(out$survey_year == 2005.5 & is.na(out$ref_year_mean)))
})

test_that('one_point_adjusted_with_gdp() works as expected', {
  out <- one_point_adjusted_with_gdp(svy_year = 2005.5, svy_gdp = 90, ref_gdp = 100, svy_mean = 1000)
  expect_equal(out$survey_year, 2005.5)
  expect_equal(out$ref_year_mean, 1111.111111, tolerance = 1.5e-8)
  out <- one_point_adjusted_with_gdp(svy_year = 2005.5, svy_gdp = 90, ref_gdp = 80, svy_mean = 900)
  expect_equal(out$ref_year_mean, 800)
  expect_equal(out$survey_year, 2005.5)
})

test_that('one_point_adjusted_with_pce() works as expected', {
  out <- one_point_adjusted_with_pce(svy_year = 2005.5, svy_pce = 90, ref_pce = 100, svy_mean = 1000)
  expect_equal(out$survey_year, 2005.5)
  expect_equal(out$ref_year_mean, 1111.111111, tolerance = 1.5e-8)
  out <- one_point_adjusted_with_pce(svy_year = 2005.5, svy_pce = 90, ref_pce = 80, svy_mean = 900)
  expect_equal(out$ref_year_mean, 800)
  expect_equal(out$survey_year, 2005.5)
})

test_that('one_point_same_as_survey_year() works as expected', {
  out <- one_point_same_as_survey_year(svy_year = 2005.5, svy_mean = 1000)
  expect_equal(out$survey_year, 2005.5)
  expect_equal(out$ref_year_mean, 1000)
})

test_that('non_monotonic_adjusted_with_pce() works as expected', {
  out <- non_monotonic_adjusted_with_pce(svy_year = c(2005.5, 2010), svy_pce = c(90, 95),
                                         svy_mean = c(995,1000), ref_pce = 100)
  expect_equal(out$survey_year, c(2005.5, 2010))
  expect_equal(out$ref_year_mean, c(1105.55556, 1052.631579), tolerance = 1.5e-6)
  out <- non_monotonic_adjusted_with_pce(svy_year = c(2005.5, 2010), svy_pce = c(90, 95),
                                         svy_mean = c(990,1000), ref_pce = 80)
  expect_equal(out$survey_year, c(2005.5, 2010))
  expect_equal(out$ref_year_mean, c(880, 842.105263), tolerance = 1.5e-8)
})


test_that('non_monotonic_adjusted_with_gdp() works as expected', {
  out <- non_monotonic_adjusted_with_gdp(svy_year = c(2005.5, 2010), svy_gdp = c(90, 95),
                                         svy_mean = c(995,1000), ref_gdp = 100)
  expect_equal(out$survey_year, c(2005.5, 2010))
  expect_equal(out$ref_year_mean, c(1105.555556, 1052.631579), tolerance = 1.5e-8)
  out <- non_monotonic_adjusted_with_gdp(svy_year = c(2005.5, 2010), svy_gdp = c(90, 95),
                                         svy_mean = c(990,1000), ref_gdp = 80)
  expect_equal(out$survey_year, c(2005.5, 2010))
  expect_equal(out$ref_year_mean, c(880, 842.105263), tolerance = 1.5e-8)
})

test_that('same_direction_interpolated_with_pce() works as expected', {
  out <- same_direction_interpolated_with_pce(svy_year = 2005.5, svy_pce = c(90, 95),
                                              svy_mean = c(995,1000), ref_pce = 100)
  expect_true(all(out$survey_year == 2005.5 & out$ref_year_mean == 1005))
  out <- same_direction_interpolated_with_pce(svy_year = c(2005.5, 2010), svy_pce = c(90, 95),
                                              svy_mean = c(995,1000), ref_pce = 100)
  expect_true(all(out$survey_year %in% c(2005.5, 2010) & out$ref_year_mean == 1005))
})


test_that('same_direction_interpolated_with_gdp() works as expected', {
  out <- same_direction_interpolated_with_gdp(svy_year = 2005.5, svy_gdp = c(90, 95),
                                              svy_mean = c(995,1000), ref_gdp = 100)
  expect_true(all(out$survey_year == 2005.5 & out$ref_year_mean == 1005))
  out <- same_direction_interpolated_with_gdp(svy_year = c(2005.5, 2010), svy_gdp = c(90, 95),
                                              svy_mean = c(995,1000), ref_gdp = 100)
  expect_true(all(out$survey_year %in% c(2005.5, 2010) & out$ref_year_mean == 1005))
})

test_that('adjust_welfare_mean() works as expected', {

  df <- data.frame(adjusted_svy_gdp = c(80, 85), adjusted_svy_pce = c(100, 105),
                   svy_year_mean = c(990, 1000), survey_year = c(2005.5, 2010),
                   wb_region_code = 'ECA')

  # one_point_adjusted_with_gdp
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'one_point_adjusted_with_gdp')
  expect_equal(out$ref_year_mean, c(1014.75, 964.705882), tolerance = 1.5e-8)

  # one_point_adjusted_with_pce
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'one_point_adjusted_with_pce')
  expect_equal(out$ref_year_mean, c(1089, 1047.619048), tolerance = 1.5e-8)

  # one_point_same_as_survey_year
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'one_point_same_as_survey_year')
  expect_equal(out$ref_year_mean, c(990, 1000))

  # non_monotonic_adjusted_with_pce
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'non_monotonic_adjusted_with_pce')
  expect_equal(out$ref_year_mean, c(1089, 1047.619048), tolerance = 1.5e-8)

  # non_monotonic_adjusted_with_gdp
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'non_monotonic_adjusted_with_gdp')
  expect_equal(out$ref_year_mean, c(1014.75, 964.705882), tolerance = 1.5e-8)

  # same_direction_interpolated_with_pce
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'same_direction_interpolated_with_pce')
  expect_true(all(out$ref_year_mean == 1010))

  # same_direction_interpolated_with_gdp
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'same_direction_interpolated_with_gdp')
  expect_true(all(out$ref_year_mean == 994))

  # missing_pce_a
  out <- adjust_welfare_mean(svy_table = df, ref_pce = 110, ref_gdp = 82, method = 'missing_pce_a')
  expect_true(all(is.na(out$ref_year_mean)))
})
