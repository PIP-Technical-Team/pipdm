# Test computation of summary statistics and underlying functions

# Read in synthetic microdata files
dl <- readRDS('../testdata/synthetic-microdata.RDS')

# Data preparations
dl <- lapply(dl, function(x){
  df <- x$data
  # Order by increasing welfare values
  df <- df[order(df$welfare),]
  # Remove rows with missing welfare values
  df <- df[!is.na(df$welfare),]
  x$data <- df
  return(x)
})

# Validation checks
test_that('check_inputs_measure_weight() works as expected', {
  expect_error(check_inputs_measure_weight(measure = 1:2, weight = c(1, NA)), 'weight cannot contain missing values')
  expect_error(check_inputs_measure_weight(measure = c(1, NA), weight = 1:2), 'measure cannot contain missing values')
  expect_error(check_inputs_measure_weight(measure = 1, weight = '1'), 'weight is not a numeric or integer vector')
  expect_error(check_inputs_measure_weight(measure = '1', weight = 1), 'measure is not a numeric or integer vector')
  expect_error(check_inputs_measure_weight(measure = 1:2, weight = 1), 'measure and weight must be of the same length')
  expect_error(check_inputs_measure_weight(measure = 3:1, weight = 1:3), 'measure must be sorted in increasing order')
})

# Gini
test_that('compute_gini() returns the correct result', {
  lapply(dl, function(x){
    df <- x$data
    res <- compute_gini(measure = df$welfare, weight = df$weight)
    expect_equal(res, x$stats$gini)
  })
})

# Mean log deviation
test_that('compute_mld() returns the correct result', {
  lapply(dl, function(x){
    df <- x$data
    res <- compute_mld(measure = df$welfare, weight = df$weight)
    expect_equal(res, x$stats$mld)
  })
})

# Lorenz curve
test_that('compute_lorenz() returns the correct results', {
  lapply(dl, function(x){
    df <- x$data
    res1 <- x$stats$lorenz
    res2 <- compute_lorenz(measure = df$welfare, weight = df$weight)
    expect_identical(dim(res1), dim(res2))
    expect_equal(res1$measure, res2$measure)
    expect_equal(res1$lorenz_weight, res2$lorenz_weight)
    expect_equal(res1$lorenz_weighted_measure, res2$lorenz_weighted_measure)
  })
})

# compute_stats
test_that('compute_stats() returns the correct results', {
  lapply(dl, function(x){
    df <- x$data
    res1 <- x$stats
    res2 <- compute_stats(measure = df$welfare, weight = df$weight)
    expect_equal(res1$nobs, res2$nobs)
    expect_equal(res1$sum_weight, res2$sum_weight)
    expect_equal(res1$min_measure, res2$min_measure)
    expect_equal(res1$max_measure, res2$max_measure)
    expect_equal(res1$sum_weighted_measure, res2$sum_weighted_measure)
    expect_equal(res1$mean_weighted_measure, res2$mean_weighted_measure)
    expect_equal(res1$gini, res2$gini)
    expect_equal(res1$mld, res2$mld)
    expect_equal(res1$lorenz$measure, res2$lorenz$measure)
    expect_equal(res1$lorenz$lorenz_weight, res2$lorenz$lorenz_weight)
    expect_equal(res1$lorenz$lorenz_weighted_measure, res2$lorenz$lorenz_weighted_measure)
  })
})


