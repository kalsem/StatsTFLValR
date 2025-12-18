# tests/testthat/test-freq_by.R

testthat::test_that("freq_by: errors if id_var not found and no common alternatives exist", {
  df <- data.frame(
    TRTAN = c(1, 1, 2),
    CAT   = c("1", "2", "1"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    freq_by(
      data       = df,
      denom_data = df,
      main_group = "TRTAN",
      last_group = "CAT",
      label      = "Test",
      sec_ord    = 1,
      id_var     = "USUBJID",
      fmt        = NULL
    ),
    regexp = "Could not find ID variable",
    fixed  = FALSE
  )
})

testthat::test_that("freq_by: auto-detects ID variable when id_var missing", {
  df <- data.frame(
    SUBJID = paste0("S", 1:6),
    TRTAN  = c(1, 1, 2, 2, 1, 2),
    CAT    = c("1", "2", "1", "2", "1", "2"),
    stringsAsFactors = FALSE
  )

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 9,
    id_var     = "USUBJID",  # does not exist, should auto-detect SUBJID
    fmt        = NULL
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("stat", "sort_ord", "sec_ord") %in% names(out)))
})

testthat::test_that("freq_by: fmt=NULL derives levels from observed data (with header row and indent)", {
  df <- data.frame(
    USUBJID = paste0("S", 1:8),
    TRTAN   = c(1,1,1,1,2,2,2,2),
    CAT     = c("1","2","2","3","1","1","3","3"),
    stringsAsFactors = FALSE
  )

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "My label",
    sec_ord    = 1,
    fmt        = NULL,
    indent     = 2
  )

  # Header + 3 observed categories
  testthat::expect_equal(nrow(out), 1 + 3)
  testthat::expect_equal(out$stat[[1]], "My label")

  # Indent applied to non-header rows
  testthat::expect_true(startsWith(out$stat[[2]], "  "))
  testthat::expect_true(startsWith(out$stat[[3]], "  "))
  testthat::expect_true(startsWith(out$stat[[4]], "  "))

  # sort_ord sequential; sec_ord constant
  testthat::expect_equal(out$sort_ord, seq_len(nrow(out)))
  testthat::expect_true(all(out$sec_ord == 1))

  # Treatment columns are standardized to trt*
  testthat::expect_true(all(c("trt1", "trt2") %in% names(out)))
})

testthat::test_that("freq_by: counts distinct subjects and formats as 'n (pct)' or '0'", {
  df <- data.frame(
    USUBJID = c("S1","S1","S2","S3","S4","S5"),
    TRTAN   = c(1,  1,  1,  2,  2,  2),
    CAT     = c("A","A","B","A","B","B"),
    stringsAsFactors = FALSE
  )
  # Denoms: trt1 has S1,S2 => N=2; trt2 has S3,S4,S5 => N=3
  # CAT A: trt1 => S1 => 1 (50.0); trt2 => S3 => 1 (33.3)
  # CAT B: trt1 => S2 => 1 (50.0); trt2 => S4,S5 => 2 (66.7)

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 2,
    fmt        = NULL,
    indent     = 2
  )

  # Grab row for "A" and "B" (indented)
  rowA <- out[trimws(out$stat) == "A", , drop = FALSE]
  rowB <- out[trimws(out$stat) == "B", , drop = FALSE]

  testthat::expect_equal(nrow(rowA), 1)
  testthat::expect_equal(nrow(rowB), 1)

  testthat::expect_true(grepl("^\\s*\\d+ \\(\\s*\\d+\\.\\d\\)$|^0$", rowA$trt1[[1]]))
  testthat::expect_true(grepl("^\\s*\\d+ \\(\\s*\\d+\\.\\d\\)$|^0$", rowA$trt2[[1]]))

  testthat::expect_equal(trimws(rowA$trt1[[1]]), "1 ( 50.0)")
  testthat::expect_equal(trimws(rowA$trt2[[1]]), "1 ( 33.3)")
  testthat::expect_equal(trimws(rowB$trt1[[1]]), "1 ( 50.0)")
  testthat::expect_equal(trimws(rowB$trt2[[1]]), "2 ( 66.7)")
})

testthat::test_that("freq_by: na_to_code maps NA to code and includes it when fmt=NULL", {
  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    TRTAN   = c(1,1,1,2,2,2),
    CAT     = c("1", NA, "2", "1", NA, NA),
    stringsAsFactors = FALSE
  )

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 3,
    fmt        = NULL,
    na_to_code = "99"
  )

  # We expect "99" to appear as a category label (auto fmt: code==label)
  testthat::expect_true(any(trimws(out$stat) == "99"))
})

testthat::test_that("freq_by: explicit fmt (named vector) + include_all_fmt_levels=TRUE shows all fmt levels", {
  df <- data.frame(
    USUBJID = paste0("S", 1:4),
    TRTAN   = c(1,1,2,2),
    CAT     = c("1", "1", "2", "2"),
    stringsAsFactors = FALSE
  )

  fmt <- c("1" = "<1", "2" = "1-<4", "3" = ">=4")  # 3 not present in data

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 4,
    fmt        = fmt,
    include_all_fmt_levels = TRUE
  )

  # Expect label ">=4" to appear even though CAT code "3" absent in data
  testthat::expect_true(any(trimws(out$stat) == ">=4"))

  # And the corresponding cells should be "0"
  row3 <- out[trimws(out$stat) == ">=4", , drop = FALSE]
  testthat::expect_equal(row3$trt1[[1]], "0")
  testthat::expect_equal(row3$trt2[[1]], "0")
})

testthat::test_that("freq_by: explicit fmt (data.frame with value/raw) works", {
  df <- data.frame(
    USUBJID = paste0("S", 1:4),
    TRTAN   = c(1,1,2,2),
    CAT     = c("1", "2", "1", "2"),
    stringsAsFactors = FALSE
  )

  fmt_df <- data.frame(
    value = c("1","2"),
    raw   = c("One","Two"),
    stringsAsFactors = FALSE
  )

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 5,
    fmt        = fmt_df
  )

  testthat::expect_true(any(trimws(out$stat) == "One"))
  testthat::expect_true(any(trimws(out$stat) == "Two"))
})

testthat::test_that("freq_by: fmt as object name (string) resolves correctly", {
  df <- data.frame(
    USUBJID = paste0("S", 1:4),
    TRTAN   = c(1,1,2,2),
    CAT     = c("1", "2", "1", "2"),
    stringsAsFactors = FALSE
  )

  fmt_local <- c("1" = "One", "2" = "Two")

  out <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 6,
    fmt        = "fmt_local"
  )

  testthat::expect_true(any(trimws(out$stat) == "One"))
  testthat::expect_true(any(trimws(out$stat) == "Two"))
})

testthat::test_that("freq_by: use_sas_round flag runs and returns same shape (numeric stability check)", {
  # Use a construct that yields a 12.5% style percent to exercise rounding path.
  df <- data.frame(
    USUBJID = paste0("S", 1:8),
    TRTAN   = c(rep(1, 8)),
    CAT     = c("A", "B", "B", "B", "B", "B", "B", "B"), # A has 1/8 => 12.5
    stringsAsFactors = FALSE
  )

  out_r <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 7,
    fmt        = NULL,
    use_sas_round = FALSE
  )

  out_s <- freq_by(
    data       = df,
    denom_data = df,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 7,
    fmt        = NULL,
    use_sas_round = TRUE
  )

  # Same structure
  testthat::expect_identical(names(out_r), names(out_s))
  testthat::expect_equal(nrow(out_r), nrow(out_s))

  # Ensure the row exists and the cell is non-zero formatted
  rowA_r <- out_r[trimws(out_r$stat) == "A", , drop = FALSE]
  rowA_s <- out_s[trimws(out_s$stat) == "A", , drop = FALSE]
  testthat::expect_equal(nrow(rowA_r), 1)
  testthat::expect_equal(nrow(rowA_s), 1)

  testthat::expect_true(grepl("^\\s*1 \\(\\s*\\d+\\.\\d\\)$", rowA_r$trt1[[1]]))
  testthat::expect_true(grepl("^\\s*1 \\(\\s*\\d+\\.\\d\\)$", rowA_s$trt1[[1]]))
})

testthat::test_that("freq_by: adds missing treatment columns as '0' based on denom_data arms", {
  df_num <- data.frame(
    USUBJID = paste0("S", 1:4),
    TRTAN   = c(1,1,2,2),
    CAT     = c("A","A","A","B"),
    stringsAsFactors = FALSE
  )

  # denom has an extra arm 3 not present in data_num
  denom <- data.frame(
    USUBJID = paste0("D", 1:6),
    TRTAN   = c(1,1,2,2,3,3),
    stringsAsFactors = FALSE
  )

  out <- freq_by(
    data       = df_num,
    denom_data = denom,
    main_group = "TRTAN",
    last_group = "CAT",
    label      = "Header",
    sec_ord    = 8,
    fmt        = NULL
  )

  testthat::expect_true(all(c("trt1","trt2","trt3") %in% names(out)))

  # For rows that exist, trt3 should be "0"
  data_rows <- out[-1, , drop = FALSE]
  testthat::expect_true(all(data_rows$trt3 == "0"))
})
