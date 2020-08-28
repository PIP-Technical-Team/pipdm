# Test get_welfare_adjustment_method() and its underlying utility functions

# Helper function
create_df <- function(survey_year = c(2005.5, 2010), adjusted_svy_pce = c(100, 110),
                      adjusted_svy_gdp = c(81.5, 85), wb_region_code = 'ECA',
                      svy_year_mean = c(1000, 990), n = NULL){

  df <- data.frame(country_code = 'ABC', survey_year = survey_year,
                   adjusted_svy_pce = adjusted_svy_pce,
                   adjusted_svy_gdp = adjusted_svy_gdp,
                   svy_year_mean = svy_year_mean,
                   wb_region_code = wb_region_code)

  if (!is.null(n))
    if (n == 1) df <- df[1,]

  return(df)

}

# get_survey_side
test_that('get_survey_side() is working as expected', {
  df <- create_df(n = 1)
  df2 <- create_df()
  expect_identical(get_survey_side(df), 'one-side')
  expect_identical(get_survey_side(df2), 'both-sides')
})

# is_same_direction
test_that('is_same_direction() is working as expected', {
  expect_true(is_same_direction(x = c(1, 2), y = c(2,5)))
  expect_false(is_same_direction(x = c(2, 1), y = c(2,5)))
})

# is_monotonic
test_that('is_monotonic() is working as expected', {
  expect_true(is_monotonic(x1 = 100, x2 = 110, r = 105))
  expect_false(is_monotonic(x1 = 100, x2 = 110, r = 110))
})

# is_one_point_adjusted_with_pce
test_that('is_one_point_adjusted_with_pce() is working as expected', {
  df <- create_df(n = 1)
  expect_true(is_one_point_adjusted_with_pce(svy_table = df, ref_pce = 90))
  expect_true(is_one_point_adjusted_with_pce(svy_table = df, ref_pce = 100))
  expect_false(is_one_point_adjusted_with_pce(svy_table = df, ref_pce = NA))
  expect_false(is_one_point_adjusted_with_pce(svy_table = create_df(n = 1, adjusted_svy_pce = NA), ref_pce = 100))
  expect_false(is_one_point_adjusted_with_pce(svy_table = create_df(n = 1, wb_region_code = 'SSA'), ref_pce = 100))
})

# is_one_point_adjusted_with_gdp
test_that('is_one_point_adjusted_with_gdp() is working as expected', {
  df <- create_df(n = 1)
  expect_true(is_one_point_adjusted_with_gdp(svy_table = df, ref_gdp = 85))
  expect_false(is_one_point_adjusted_with_gdp(svy_table = df, ref_gdp = NA))
  expect_true(is_one_point_adjusted_with_gdp(svy_table = create_df(n = 1, wb_region_code = 'SSA'), ref_gdp = 85))
  expect_false(is_one_point_adjusted_with_gdp(svy_table = create_df(n = 1, adjusted_svy_gdp = NA), ref_gdp = 85))
})

# is_non_monotonic_adjusted_with_pce
test_that('is_non_monotonic_adjusted_with_pce() is working as expected', {
  expect_false(is_non_monotonic_adjusted_with_pce(create_df(n = 1)))
  expect_false(is_non_monotonic_adjusted_with_pce(create_df()))
  expect_false(is_non_monotonic_adjusted_with_pce(create_df(), ref_pce = NA))
  expect_false(is_non_monotonic_adjusted_with_pce(svy_table = create_df(adjusted_svy_pce = NA), ref_pce = 100))
  expect_false(is_non_monotonic_adjusted_with_pce(svy_table = create_df(wb_region_code = 'SSA'), ref_pce = 100))
  expect_true(is_non_monotonic_adjusted_with_pce(create_df(), ref_pce = 100))
  expect_true(is_non_monotonic_adjusted_with_pce(svy_table = create_df(wb_region_code = 'ECA'), ref_pce = 100))
})

# is_same_direction_interpolated_with_pce
test_that('is_same_direction_interpolated_with_pce() is working as expected', {
  expect_false(is_same_direction_interpolated_with_pce(create_df(n = 1), ref_pce = 100))
  expect_false(is_same_direction_interpolated_with_pce(create_df(), ref_pce = 100))
  expect_false(is_same_direction_interpolated_with_pce(create_df(), ref_pce = NA))
  expect_false(is_same_direction_interpolated_with_pce(create_df(svy_year_mean = c(990, 1000)), ref_pce = 100))
  expect_true(is_same_direction_interpolated_with_pce(create_df(svy_year_mean = c(990, 1000)), ref_pce = 105))
  expect_false(is_same_direction_interpolated_with_pce(
    create_df(svy_year_mean = c(990, 1000), adjusted_svy_pce = NA),
    ref_pce = 105)
  )
  expect_false(is_same_direction_interpolated_with_pce(
    create_df(svy_year_mean = c(990, 1000), wb_region_code = 'SSA'), ref_pce = 105)
  )
  expect_false(is_same_direction_interpolated_with_pce(
    create_df(svy_year_mean = c(1000, 990)), ref_pce = 105)
  )
  # What about second TRUE argument?
})

# is_non_monotonic_adjusted_with_gdp
test_that('is_non_monotonic_adjusted_with_gdp() is working as expected', {
  expect_false(is_non_monotonic_adjusted_with_gdp(create_df(n = 1), ref_gdp = 82))
  expect_false(is_non_monotonic_adjusted_with_gdp(create_df(n = 1), ref_gdp = NA))
  expect_false(is_non_monotonic_adjusted_with_gdp(create_df(n = 1, adjusted_svy_gdp = NA), ref_gdp = 82))
  expect_false(is_non_monotonic_adjusted_with_gdp(create_df(svy_year_mean = c(990, 1000)), ref_gdp = 82))
  expect_true(is_non_monotonic_adjusted_with_gdp(create_df(), ref_gdp = 82))
  expect_true(is_non_monotonic_adjusted_with_gdp(create_df(svy_year_mean = c(990, 1000)), ref_gdp = 85))
})

# is_same_direction_interpolated_with_gdp
test_that('is_same_direction_interpolated_with_gdp() is working as expected', {
  expect_false(is_same_direction_interpolated_with_gdp(create_df(n = 1), ref_gdp = 82))
  expect_false(is_same_direction_interpolated_with_gdp(create_df(), ref_gdp = 82))
  expect_false(is_same_direction_interpolated_with_gdp(create_df(), ref_gdp = NA))
  expect_false(is_same_direction_interpolated_with_gdp(
    create_df(svy_year_mean = c(990, 1000), adjusted_svy_gdp = NA), ref_gdp = 82)
  )
  expect_false(is_same_direction_interpolated_with_gdp(create_df(svy_year_mean = c(990, 1000)), ref_gdp = NA))
  expect_false(is_same_direction_interpolated_with_gdp(create_df(svy_year_mean = c(990, 1000)), ref_gdp = 85))
  expect_true(is_same_direction_interpolated_with_gdp(create_df(svy_year_mean = c(990, 1000)), ref_gdp = 82))
})

#
test_that('get_welfare_adjustment_method() is working correctly',{
  # Reference year equal to the survey mean
  expect_identical(
    get_welfare_adjustment_method(
      create_df(),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'one_point_same_as_survey_year')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(n = 1, survey_year = 2010),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'one_point_same_as_survey_year')

  # First observation of survey mean missing (A)
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(NA, 1000)),
      ref_year = 2007, ref_gdp = 105, ref_pce = 82),
    'missing_mean_a')

  # Second observation of survey mean missing (B)
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(1000, NA)),
      ref_year = 2007, ref_gdp = 105, ref_pce = 82),
    'missing_mean_b')

  # Reference year equal to survey year when the first observation of
  # the survey mean is missing.
  # AE note: Is this the correct behaviour?
  # Should the ref_year == survey_year check be done before
  # the is.na(svy_table[['svy_year_mean']][1])) check?
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(NA, 1000)),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'missing_mean_a')

  # Reference year equal to survey year when the second observation of
  # the survey mean is missing.
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(1000, NA)),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'one_point_same_as_survey_year')

  # One-side adjusted with PCE
  expect_identical(
    get_welfare_adjustment_method(
      create_df(n = 1),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'one_point_adjusted_with_pce')

  # One-side adjusted with GDP
  expect_identical(
    get_welfare_adjustment_method(
      create_df(n = 1, adjusted_svy_pce = NA),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'one_point_adjusted_with_gdp')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(n = 1, wb_region_code = 'SSA'),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'one_point_adjusted_with_gdp')

  # One-side missing PCE
  expect_identical(
    get_welfare_adjustment_method(
      create_df(n = 1, adjusted_svy_pce = NA, adjusted_svy_gdp = NA),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'missing_pce_a')

  # One-side missing GDP
  expect_identical(
    get_welfare_adjustment_method(
      create_df(n = 1, wb_region_code = 'SSA', adjusted_svy_gdp = NA),
      ref_year = 2010, ref_gdp = 105, ref_pce = 82),
    'missing_gdp_a')

  # Both sides non-monotonic adjusted with PCE
  expect_identical(
    get_welfare_adjustment_method(
      create_df(),
      ref_year = 2007, ref_gdp = 105, ref_pce = 82),
    'non_monotonic_adjusted_with_pce')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(),
      ref_year = 2007, ref_gdp = 105, ref_pce = 100),
    'non_monotonic_adjusted_with_pce')

  # Both sides non-monotonic adjusted with GDP
  expect_identical(
    get_welfare_adjustment_method(
      create_df(adjusted_svy_pce = c(NA, 110)),
      ref_year = 2007, ref_gdp = 105, ref_pce = 82),
    'non_monotonic_adjusted_with_gdp')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(wb_region_code = 'SSA'),
      ref_year = 2007, ref_gdp = 105, ref_pce = 82),
    'non_monotonic_adjusted_with_gdp')

  # Same direction interpolated with PCE
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(990, 1000)),
      ref_year = 2007, ref_gdp = 105, ref_pce = 105),
    'same_direction_interpolated_with_pce')

  # Same direction interpolated with GDP
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(990, 1000), adjusted_svy_pce = c(NA, 90)),
      ref_year = 2007, ref_gdp = 82, ref_pce = 105),
    'same_direction_interpolated_with_gdp')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(svy_year_mean = c(990, 1000), wb_region_code = 'SSA'),
      ref_year = 2007, ref_gdp = 82, ref_pce = 105),
    'same_direction_interpolated_with_gdp')

  # Both sides missing PCE
  expect_identical(
    get_welfare_adjustment_method(
      create_df(adjusted_svy_pce = c(NA, 110), adjusted_svy_gdp = NA),
      ref_year = 2007, ref_gdp = 82, ref_pce = 105),
    'missing_pce_a')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(adjusted_svy_pce = c(100, NA), adjusted_svy_gdp = NA),
      ref_year = 2007, ref_gdp = 82, ref_pce = 105),
    'missing_pce_b')

  # Both sides missing GDP
  expect_identical(
    get_welfare_adjustment_method(
      create_df(adjusted_svy_gdp = c(NA, 85), wb_region_code = 'SSA'),
      ref_year = 2007, ref_gdp = 82, ref_pce = 105),
    'missing_gdp_a')
  expect_identical(
    get_welfare_adjustment_method(
      create_df(adjusted_svy_gdp = c(81.5, NA), wb_region_code = 'SSA'),
      ref_year = 2007, ref_gdp = 82, ref_pce = 105),
    'missing_gdp_b')
})
