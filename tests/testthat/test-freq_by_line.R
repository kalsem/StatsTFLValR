# tests/testthat/test-freq_by_line.R

testthat::test_that("freq_by_line: basic counts/percents with internal denominator (numeric-like TRT -> trt* columns)", {
  df <- data.frame(
    USUBJID = paste0("S", 1:10),
    TRT01P  = c("0","0","54","54","54","100","100","100","100","100"),
    SAFFL   = c("Y","N","Y","Y","N","Y","N","Y","Y","N"),
    AGE     = c(65, 40, 70, 30, 55, 66, 50, 80, 19, 61),
    stringsAsFactors = FALSE
  )

  out <- freq_by_line(df, USUBJID, TRT01P, SAFFL == "Y", label = "SAF population")

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 1)

  testthat::expect_true("stat" %in% names(out))
  testthat::expect_true(all(c("trt0","trt54","trt100") %in% names(out)))

  testthat::expect_equal(out$stat[[1]], "SAF population")
  testthat::expect_equal(out$trt0[[1]],   "1 (50.0)")   # 1/2
  testthat::expect_equal(out$trt54[[1]],  "2 (66.7)")   # 2/3
  testthat::expect_equal(out$trt100[[1]], "3 (60.0)")   # 3/5
})

testthat::test_that("freq_by_line: supports external denominator (denom_data) and percent changes accordingly", {
  df <- data.frame(
    USUBJID = paste0("S", 1:10),
    TRT01P  = c("0","0","54","54","54","100","100","100","100","100"),
    SAFFL   = c("Y","N","Y","Y","N","Y","N","Y","Y","N"),
    AGE     = c(65, 40, 70, 30, 55, 66, 50, 80, 19, 61),
    stringsAsFactors = FALSE
  )

  saf <- df[df$SAFFL == "Y", ]

  out <- freq_by_line(df, USUBJID, TRT01P, AGE >= 65, label = "Age >=65 in SAF", denom_data = saf)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(all(c("trt0","trt54","trt100") %in% names(out)))

  # SAF denominators: trt0=1 (S1), trt54=2 (S3,S4), trt100=3 (S6,S8,S9)
  # AGE>=65 numerators (from full df): trt0=1 (S1), trt54=1 (S3), trt100=2 (S6,S8)
  testthat::expect_equal(out$stat[[1]], "Age >=65 in SAF")
  testthat::expect_equal(out$trt0[[1]],   "1 (100.0)")
  testthat::expect_equal(out$trt54[[1]],  "1 (50.0)")
  testthat::expect_equal(out$trt100[[1]], "2 (66.7)")
})

testthat::test_that("freq_by_line: returns '0' when no subjects meet filter in an arm", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRT01P  = c("0","0","54","54","100","100"),
    SAFFL   = c("N","N","Y","N","N","N"),
    stringsAsFactors = FALSE
  )

  out <- freq_by_line(df, USUBJID, TRT01P, SAFFL == "Y", label = "SAF population")

  testthat::expect_true(all(c("trt0","trt54","trt100") %in% names(out)))
  testthat::expect_equal(out$trt0[[1]],   "0")
  testthat::expect_equal(out$trt54[[1]],  "1 (50.0)")
  testthat::expect_equal(out$trt100[[1]], "0")
})

testthat::test_that("freq_by_line: treatment ordering is numeric when arms are numeric-like", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRT01P  = c("100","0","54","0","100","54"),
    SAFFL   = c("Y","Y","Y","N","N","N"),
    stringsAsFactors = FALSE
  )

  out <- freq_by_line(df, USUBJID, TRT01P, SAFFL == "Y", label = "SAF population")
  testthat::expect_identical(names(out), c("stat","trt0","trt54","trt100"))
})

testthat::test_that("freq_by_line: non-numeric treatment arms are NOT prefixed and are alphabetically ordered", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    ARM     = c("Placebo","DrugA","DrugB","Placebo","DrugB","DrugA"),
    FLAG    = c("Y","N","Y","Y","N","N"),
    stringsAsFactors = FALSE
  )

  out <- freq_by_line(df, USUBJID, ARM, FLAG == "Y", label = "FLAG=Y")

  testthat::expect_true(all(c("DrugA","DrugB","Placebo") %in% names(out)))
  testthat::expect_false(any(grepl("^trt", names(out)[names(out) != "stat"])))
  testthat::expect_identical(names(out), c("stat","DrugA","DrugB","Placebo"))
})

testthat::test_that("freq_by_line: trims whitespace in treatment values (normalization)", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRT01P  = c(" 0", "0 ", " 54 ", "54", "100", "100 "),
    SAFFL   = c("Y","N","Y","N","Y","N"),
    stringsAsFactors = FALSE
  )

  out <- freq_by_line(df, USUBJID, TRT01P, SAFFL == "Y", label = "SAF population")

  testthat::expect_true(all(c("trt0","trt54","trt100") %in% names(out)))
  testthat::expect_equal(out$trt0[[1]],   "1 (50.0)")
  testthat::expect_equal(out$trt54[[1]],  "1 (50.0)")
  testthat::expect_equal(out$trt100[[1]], "1 (50.0)")
})

testthat::test_that("freq_by_line: counts DISTINCT subjects within arm (duplicates do not inflate n)", {
  df <- data.frame(
    USUBJID = c("S1","S1","S2","S3","S3","S4"),
    TRT01P  = c("0","0","0","54","54","54"),
    FLAG    = c("Y","Y","Y","Y","Y","N"),
    stringsAsFactors = FALSE
  )

  out <- freq_by_line(df, USUBJID, TRT01P, FLAG == "Y", label = "FLAG=Y")

  testthat::expect_true(all(c("trt0","trt54") %in% names(out)))
  testthat::expect_equal(out$trt0[[1]],  "2 (100.0)")
  testthat::expect_equal(out$trt54[[1]], "1 (50.0)")
})

testthat::test_that("freq_by_line: current implementation errors if treatment has empty-string level (pivot_wider disallows empty colname)", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRT01P  = c("0", NA, "54", "", "100", "0"),
    SAFFL   = c("Y","Y","N","Y","Y","N"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    freq_by_line(df, USUBJID, TRT01P, SAFFL == "Y", label = "SAF population"),
    regexp = "empty string|Can't subset columns|pivot_wider",
    fixed  = FALSE
  )
})

testthat::test_that("freq_by_line: does not error when denom_data has extra columns or different row order", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRT01P  = c("0","0","54","54","100","100"),
    FLAG    = c("Y","N","Y","N","Y","N"),
    stringsAsFactors = FALSE
  )

  denom <- df
  denom$EXTRA <- 999
  denom <- denom[sample.int(nrow(denom)), ]

  testthat::expect_no_error({
    out <- freq_by_line(df, USUBJID, TRT01P, FLAG == "Y", label = "FLAG=Y", denom_data = denom)
  })
})
