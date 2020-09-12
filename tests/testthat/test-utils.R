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
