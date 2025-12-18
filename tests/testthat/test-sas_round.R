# tests/testthat/test-sas_round.R

testthat::test_that("sas_round: rounds halves away from zero (digits = 0)", {
  x <- c(1.5, 2.5, 3.5, -1.5, -2.5, -3.5, 0.5, -0.5)
  exp <- c(2,   3,   4,   -2,   -3,   -4,   1,   -1)

  testthat::expect_equal(sas_round(x), exp)
})

testthat::test_that("sas_round: differs from base R bankers rounding at .5 ties", {
  # Base R bankers rounding
  testthat::expect_equal(round(c(2.5, 3.5)), c(2, 4))

  # SAS-style: halves away from zero
  testthat::expect_equal(sas_round(c(2.5, 3.5)), c(3, 4))
  testthat::expect_equal(sas_round(-2.5), -3)
})

testthat::test_that("sas_round: rounds correctly to 1 decimal place", {
  x   <- c(1.25, 1.35, -1.25, -1.35)
  exp <- c(1.3,  1.4,  -1.3,  -1.4)

  testthat::expect_equal(sas_round(x, digits = 1), exp)
})

testthat::test_that("sas_round: rounds correctly to 2 decimal places", {
  x   <- c(1.235, 1.245, -1.235, -1.245)
  exp <- c(1.24,  1.25,  -1.24,  -1.25)

  testthat::expect_equal(sas_round(x, digits = 2), exp)
})

testthat::test_that("sas_round: rounds correctly to 3+ decimal places (tie handling)", {
  testthat::expect_equal(sas_round(c(1.2345, 1.2355), digits = 3), c(1.235, 1.236))
  testthat::expect_equal(sas_round(c(1.23445, 1.23455), digits = 4), c(1.2345, 1.2346))
  testthat::expect_equal(sas_round(c(1.234445, 1.234455), digits = 5), c(1.23445, 1.23446))
})

testthat::test_that("sas_round: supports negative digits (round to tens/hundreds) with half away from zero", {
  x   <- c(15, 25, 35, -15, -25, -35)
  exp <- c(20, 30, 40, -20, -30, -40)

  testthat::expect_equal(sas_round(x, digits = -1), exp)
})

testthat::test_that("sas_round: is vectorized and preserves length", {
  x <- seq(-5.5, 5.5, by = 0.5)
  out <- sas_round(x)

  testthat::expect_equal(length(out), length(x))
  testthat::expect_true(is.numeric(out))
})

testthat::test_that("sas_round: handles NA/NaN/Inf without error; preserves NA; non-finite stay non-finite or NA", {
  x <- c(NA_real_, NaN, Inf, -Inf, 1.5)

  testthat::expect_no_error({
    out <- sas_round(x)
  })

  out <- sas_round(x)

  # NA should remain NA
  testthat::expect_true(is.na(out[1]))

  # NaN may remain NaN or become NA depending on ifelse/coercion
  testthat::expect_true(is.nan(out[2]) || is.na(out[2]))

  # Inf/-Inf may remain infinite or become NA depending on ifelse/coercion
  testthat::expect_true(is.infinite(out[3]) || is.na(out[3]))
  testthat::expect_true(is.infinite(out[4]) || is.na(out[4]))

  # Finite value still correct
  testthat::expect_equal(out[5], 2)
})

testthat::test_that("sas_round: does not change non-tie values compared to base round at same digits", {
  # Values not at exactly .5 ties should match base round
  x <- c(1.24, 1.26, -1.24, -1.26, 123.456, -123.456)

  testthat::expect_equal(sas_round(x, 1), round(x, 1))
  testthat::expect_equal(sas_round(x, 2), round(x, 2))
})

testthat::test_that("sas_round: common decimal ties behave as expected (subject to floating representation)", {
  # These often behave as ties after scaling; if you ever see flakiness,
  # harden function tie detection with a tolerance.
  testthat::expect_equal(sas_round(c(0.05, -0.05), digits = 1), c(0.1, -0.1))
  testthat::expect_equal(sas_round(c(0.005, -0.005), digits = 2), c(0.01, -0.01))
})
