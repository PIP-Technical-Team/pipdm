# Test computation of summary statistics and underlying functions
library(purrr)

# Read in synthetic microdata files
files <- list.files('../testdata/', pattern = 'synthetic-microdata', full.names = TRUE)
dl <- lapply(files, readRDS)

# Data preparations
dl <- lapply(dl, function(x){
  # Order by increasing welfare values
  x <- x[order(x$welfare),]
  # Remove rows with missing welfare values
  x <- x[!is.na(x$welfare),]
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
  lapply(dl, function(df){
    res1 <- pcndm::compute_pcb_stats(y = df$welfare, w = df$weight)$gini
    res2 <- compute_gini(y = df$welfare, w = df$weight)
    expect_equal(res1, res2)
  })
})

# Mean log deviation
test_that('compute_mld() returns the same result as pcndm::compute_pcb_stats()', {
  lapply(dl, function(df){
    res1 <- pcndm::compute_pcb_stats(y = df$welfare, w = df$weight)$mld
    res2 <- compute_mld(y = df$welfare, w = df$weight)
    expect_equal(res1, res2)
  })
})

# Mean log deviation
test_that('compute_lorenz() returns the same result as pcndm::create_rpcb()', {
  lapply(dl, function(df){
    res1 <- pcndm::create_rpcb(
      df, n_observations = nrow(df),
      survey_time = unique(df$survey_year),
      year = unique(df$survey_year)) %>%
        pcndm::format_pcb() %>%
      purrr::pluck('lorenz')
    res2 <- compute_lorenz(y = df$welfare, w = df$weight)
    expect_identical(dim(res1), dim(res2))
    expect_true(all(names(res1) %in% names(res2)))
    expect_equal(res1$y, res2$y)
    expect_equal(res1$y, res2$y)
    expect_equal(res1$lorenzW, res2$lorenzW)
    expect_equal(res1$lorenzY, res2$lorenzY)
  })
})

# All summary statistics
test_that('compute_stats() returns the same result as pcndm::compute_pcb_stats()', {
  lapply(dl, function(df){
    res1 <- pcndm::compute_pcb_stats(y = df$welfare, w = df$weight)
    res2 <- compute_stats(y = df$welfare, w = df$weight)
    expect_identical(length(res1), length(res2))
    expect_true(all(names(res1) %in% names(res2)))
    expect_equal(res1$nobs, res2$nobs)
    expect_equal(res1$m, res2$m)
    expect_equal(res1$sumW, res2$sumW)
    expect_equal(res1$sumY, res2$sumY)
    expect_equal(res1$minY, res2$minY)
    expect_equal(res1$maxY, res2$maxY)
    expect_equal(res1$meanY, res2$meanY)
    expect_equal(res1$gini, res2$gini)
    expect_equal(res1$mld, res2$mld)
  })
})

test_that('compute_stats() returns the same result as pcndm::create_rpcb() for the Lorenz curve', {
  lapply(dl, function(df){
    res1 <- pcndm::create_rpcb(
      df, n_observations = nrow(df),
      survey_time = unique(df$survey_year),
      year = unique(df$survey_year)) %>%
      pcndm::format_pcb() %>%
      purrr::pluck('lorenz')
    res2 <- compute_stats(y = df$welfare, w = df$weight)$lorenz
    expect_identical(dim(res1), dim(res2))
    expect_true(all(names(res1) %in% names(res2)))
    expect_equal(res1$y, res2$y)
    expect_equal(res1$y, res2$y)
    expect_equal(res1$lorenzW, res2$lorenzW)
    expect_equal(res1$lorenzY, res2$lorenzY)
  })
})
