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
test_that('check_yw_input() works as expected', {
  expect_error(check_yw_input(y = 1:2, w = c(1, NA)), 'w cannot contain missing values')
  expect_error(check_yw_input(y = c(1, NA), w = 1:2), 'y cannot contain missing values')
  expect_error(check_yw_input(y = 1, w = '1'), 'w is not a numeric or integer vector')
  expect_error(check_yw_input(y = '1', w = 1), 'y is not a numeric or integer vector')
  expect_error(check_yw_input(y = 1:2, w = 1), 'y and w must be of the same length')
  expect_error(check_yw_input(y = 3:1, w = 1:3), 'y must be sorted in increasing order')
})

# Gini
test_that('compute_gini() returns the same result as pcndm::compute_pcb_stats()', {
  lapply(dl, function(x){
    df <- x$data
    res <- compute_gini(y = df$welfare, w = df$weight)
    expect_equal(res, x$stats$gini)
  })
})

# Mean log deviation
test_that('compute_mld() returns the same result as pcndm::compute_pcb_stats()', {
  lapply(dl, function(x){
    df <- x$data
    res <- compute_mld(y = df$welfare, w = df$weight)
    expect_equal(res, x$stats$mld)
  })
})

# Lorenz curve
test_that('compute_lorenz() returns the same result as pcndm::create_rpcb()', {
  lapply(dl, function(x){
    df <- x$data
    res1 <- x$stats$lorenz
    res2 <- compute_lorenz(y = df$welfare, w = df$weight)
    expect_identical(dim(res1), dim(res2))
    expect_equal(res1$y, res2$y)
    expect_equal(res1$y, res2$y)
    expect_equal(res1$lorenzW, res2$lorenzW)
    expect_equal(res1$lorenzY, res2$lorenzY)
  })
})

# compute_stats
test_that('compute_stats() returns the same result as pcndm::compute_pcb_stats()', {
  lapply(dl, function(x){
    df <- x$data
    res1 <- x$stats
    res2 <- compute_stats(y = df$welfare, w = df$weight)
    # expect_identical(length(res1), length(res2))
    # expect_true(all(names(res1) %in% names(res2)))
    # expect_equal(res1$nobs, res2$nobs)
    # expect_equal(res1$m, res2$m)
    # expect_equal(res1$sumW, res2$sumW)
    # expect_equal(res1$sumY, res2$sumY)
    # expect_equal(res1$minY, res2$minY)
    # expect_equal(res1$maxY, res2$maxY)
    expect_equal(res1$weighted_mean, res2$meanY)
    expect_equal(res1$gini, res2$gini)
    expect_equal(res1$mld, res2$mld)
  })
})


