# tests/testthat/test-mean_by.R

# Helper: pull a single cell from output by row label and column
.cell <- function(out, row_label, col) {
  x <- out[out$stat == row_label, col, drop = TRUE]
  unname(x)[1]
}

testthat::test_that("mean_by: errors when ID variable cannot be found / auto-detected", {
  df <- data.frame(
    TRTAN = c(1, 1, 2, 2),
    BMIBL = c(25.1, 26.3, 24.8, 23.4),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    mean_by(
      data      = df,
      group_var = TRTAN,
      uniq_var  = BMIBL,
      label     = "BMI (kg/m^2)",
      sec_ord   = 1,
      id_var    = "USUBJID"
    ),
    regexp = "Could not find ID variable",
    fixed  = FALSE
  )
})

testthat::test_that("mean_by: auto-detects ID variable (SUBJID) when USUBJID missing", {
  df <- data.frame(
    SUBJID = paste0("S", 1:6),
    TRTAN  = c(1, 1, 2, 2, 3, 3),
    BMIBL  = c(25.1, 26.3, 24.8, NA, 23.4, 27.6),
    stringsAsFactors = FALSE
  )

  out <- mean_by(
    data      = df,
    group_var = TRTAN,
    uniq_var  = BMIBL,
    label     = "BMI (kg/m^2)",
    sec_ord   = 1,
    id_var    = "USUBJID"  # will be auto-detected
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("stats","stat","sort_ord","sec_ord") %in% names(out)))

  # Label row exists
  testthat::expect_equal(out$stat[[1]], "BMI (kg/m^2)")
  testthat::expect_true(all(out$sec_ord == 1))
})

testthat::test_that("mean_by: returns expected structure and labels; indent applied", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRTAN   = c(1, 1, 2, 2, 3, 3),
    BMIBL   = c(25.1, 26.3, 24.8, NA, 23.4, 27.6),
    stringsAsFactors = FALSE
  )

  out <- mean_by(
    data      = df,
    group_var = TRTAN,
    uniq_var  = BMIBL,
    label     = "BMI (kg/m^2)",
    sec_ord   = 10,
    indent    = 3
  )

  testthat::expect_equal(out$stat[[1]], "BMI (kg/m^2)")
  testthat::expect_true(all(out$sec_ord == 10))

  # Expected stat rows exist (with indent)
  exp_rows <- c(
    "   N", "   MEAN", "   SD", "   MEDIAN", "   Q1, Q3", "   MIN, MAX"
  )
  testthat::expect_true(all(exp_rows %in% out$stat))

  # Treatment columns exist and are prefixed trt*
  testthat::expect_true(all(c("trt1","trt2","trt3") %in% names(out)))
})

testthat::test_that("mean_by: computes correct N and formatted outputs with forced precision", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRTAN   = c(1, 1, 2, 2, 3, 3),
    BMIBL   = c(25.1, 26.3, 24.8, NA, 23.4, 27.6),
    stringsAsFactors = FALSE
  )

  out <- mean_by(
    data      = df,
    group_var = TRTAN,
    uniq_var  = BMIBL,
    label     = "BMI (kg/m^2)",
    sec_ord   = 1,
    precision_override = 1,   # precision=1 => MEAN uses 2 decimals, SD uses 3, MIN/MAX uses 1, Q1/Q3 uses 2, MEDIAN uses 2
    use_sas_round = FALSE
  )

  # N per arm (NA values filtered out before group summarise)
  testthat::expect_equal(.cell(out, "   N", "trt1"), "2")
  testthat::expect_equal(.cell(out, "   N", "trt2"), "1")
  testthat::expect_equal(.cell(out, "   N", "trt3"), "2")

  # Spot-check formats (not exact numeric values to avoid platform differences in quantile type edge cases)
  testthat::expect_true(grepl("^\\d+\\.\\d{2}$", .cell(out, "   MEAN", "trt1")))
  testthat::expect_true(grepl("^\\d+\\.\\d{3}$", .cell(out, "   SD",   "trt1")))
  testthat::expect_true(grepl("^\\d+\\.\\d{2}$", .cell(out, "   MEDIAN", "trt1")))
  testthat::expect_true(grepl("^\\d+\\.\\d{2}, \\d+\\.\\d{2}$", .cell(out, "   Q1, Q3", "trt1")))
  testthat::expect_true(grepl("^\\d+\\.\\d{1}, \\d+\\.\\d{1}$", .cell(out, "   MIN, MAX", "trt1")))
})

testthat::test_that("mean_by: precision_override validation", {
  df <- data.frame(
    USUBJID = paste0("S", 1:4),
    TRTAN   = c(1, 1, 2, 2),
    BMIBL   = c(25.1, 26.3, 24.8, 23.4),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    mean_by(df, TRTAN, BMIBL, "BMI", sec_ord = 1, precision_override = -1),
    regexp = "precision_override",
    fixed  = FALSE
  )

  testthat::expect_error(
    mean_by(df, TRTAN, BMIBL, "BMI", sec_ord = 1, precision_override = "A"),
    regexp = "precision_override",
    fixed  = FALSE
  )
})

testthat::test_that("mean_by: use_sas_round affects tie rounding for mean (controlled data)", {
  # Construct data where mean is exactly x.25 so rounding at 1 decimal differs:
  # mean = 1.25 => SAS-style to 1dp => 1.3; base round to 1dp => 1.2 (bankers)
  df <- data.frame(
    USUBJID = paste0("S", 1:4),
    TRTAN   = c(1, 1, 1, 1),
    X       = c(1.0, 1.0, 1.0, 2.0),  # mean = 1.25
    stringsAsFactors = FALSE
  )

  out_r <- mean_by(
    data      = df,
    group_var = TRTAN,
    uniq_var  = X,
    label     = "X",
    sec_ord   = 1,
    precision_override = 0,  # precision=0 => MEAN uses 1 decimal
    use_sas_round = FALSE
  )

  out_s <- mean_by(
    data      = df,
    group_var = TRTAN,
    uniq_var  = X,
    label     = "X",
    sec_ord   = 1,
    precision_override = 0,
    use_sas_round = TRUE
  )

  # MEAN is formatted with 1 decimal (precision+1)
  mean_r <- .cell(out_r, "   MEAN", "trt1")
  mean_s <- .cell(out_s, "   MEAN", "trt1")

  testthat::expect_equal(mean_r, "1.2")
  testthat::expect_equal(mean_s, "1.3")
})

testthat::test_that("mean_by: missing treatment groups are filled with '0' columns when present in data levels but absent after filtering NA", {
  # Here, TRT=2 exists but all values are NA -> group is absent from summarise result,
  # function should still add trt2 column filled with '0'.
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRTAN   = c(1, 1, 2, 2, 3, 3),
    BMIBL   = c(10.1, 10.2, NA, NA, 20.1, 20.2),
    stringsAsFactors = FALSE
  )

  out <- mean_by(
    data      = df,
    group_var = TRTAN,
    uniq_var  = BMIBL,
    label     = "BMI",
    sec_ord   = 1,
    precision_override = 0
  )

  testthat::expect_true(all(c("trt1","trt2","trt3") %in% names(out)))

  # For TRT=2, all rows should be "0"
  testthat::expect_true(all(out$trt2 == "0"))
})

testthat::test_that("mean_by: string inputs for group_var/uniq_var are not supported (expect error)", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRTAN   = c(1, 1, 2, 2, 3, 3),
    BMIBL   = c(25.1, 26.3, 24.8, NA, 23.4, 27.6),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    mean_by(
      data      = df,
      group_var = "TRTAN",
      uniq_var  = "BMIBL",
      label     = "BMI",
      sec_ord   = 1,
      precision_override = 1
    ),
    regexp = "non-numeric argument|must be numeric|could not find",
    fixed  = FALSE
  )
})

