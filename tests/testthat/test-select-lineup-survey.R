library(tibble)

test_that('select_lineup_survey() always returns a data.frame',{
  df <- data.frame(survey_year = 2005, data_type = 'C')
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_equal(class(out), 'data.frame')
  df <- data.frame(survey_year = c(2005, 2010), data_type = c('I', 'C'))
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_equal(class(out), 'data.frame')
  df <- tibble::tibble(survey_year = 2005, data_type = 'C')
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_equal(class(out), 'data.frame')
})

test_that('select_lineup_survey() works as expected', {

  # Return as is if there is only a single survey to choose from
  df <- data.frame(survey_year = 2005, data_type = 'C')
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_equal(out, df)

  # Reference year equal to the survey year
  # Select consumption estimate if there are two surveys
  df <- data.frame(survey_year = c(2005, 2005), data_type = c('C', 'I'))
  out <- select_lineup_survey(df, ref_year = 2005)
  expect_true(all(out$survey_year == 2005 & out$data_type == 'C'))
  # Otherwise select the survey at the reference year
  df <- data.frame(survey_year = c(2005, 2010), data_type = c('C', 'I'))
  out <- select_lineup_survey(df, ref_year = 2010)
  expect_true(all(out$survey_year == 2010 & out$data_type == 'I'))

  # Both sides - Income
  # Keep both
  df <- data.frame(survey_year = c(2005, 2010), data_type = c('I', 'I'))
  out <- select_lineup_survey(df, ref_year = 2009)
  expect_equal(out, df)

  # Both sides - Consumption
  # Keep both
  df <- data.frame(survey_year = c(2005, 2010), data_type = c('C', 'C'))
  out <- select_lineup_survey(df, ref_year = 2009)
  expect_equal(out, df)

  # Both sides - Income and Consumption
  # Use estimate closest to the line-up year
  df <- data.frame(survey_year = c(2005, 2010), data_type = c('I', 'C'))
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_true(all(out$survey_year == 2005, out$data_type == 'I'))
  out <- select_lineup_survey(df, ref_year = 2008)
  expect_true(all(out$survey_year == 2010, out$data_type == 'C'))

  # Both sides - Income and Consumption with equal distance
  # Use consumption estimate
  df <- data.frame(survey_year = c(2005.5, 2008.5), data_type = c('I', 'C'))
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_true(all(out$survey_year == 2008.5, out$data_type == 'C'))
  df <- data.frame(survey_year = c(2005.5, 2008.5), data_type = c('C', 'I'))
  out <- select_lineup_survey(df, ref_year = 2007)
  expect_true(all(out$survey_year == 2005.5, out$data_type == 'C'))
})
