testthat::test_that("SOCbyPT: validation errors for missing required columns", {

  # minimal inputs
  indata <- data.frame(
    USUBJID   = c("S1","S2"),
    TRTAN    = c(1,2),
    AEBODSYS  = c("SOC1","SOC1"),
    AEDECOD   = c("PT1","PT2"),
    stringsAsFactors = FALSE
  )

  dmdata <- data.frame(
    USUBJID = c("S1","S2"),
    TRTAN  = c(1,2),
    stringsAsFactors = FALSE
  )

  # group_vars must be length 3 (stopifnot triggers)
  testthat::expect_error(
    SOCbyPT(indata = indata, dmdata = dmdata, group_vars = c("TRTAN","AEBODSYS"), trtan_coln = 1),
    regexp = "length\\(group_vars\\) == 3|stopifnot",
    fixed  = FALSE
  )

  # missing columns in indata
  in_bad <- indata
  in_bad$AEDECOD <- NULL
  testthat::expect_error(
    SOCbyPT(indata = in_bad, dmdata = dmdata, group_vars = c("TRTAN","AEBODSYS","AEDECOD"), trtan_coln = 1),
    regexp = "Columns missing in 'indata':",
    fixed  = FALSE
  )

  # missing columns in dmdata (treatment column)
  dm_bad <- dmdata
  dm_bad$TRTAN <- NULL
  testthat::expect_error(
    SOCbyPT(indata = indata, dmdata = dm_bad, group_vars = c("TRTAN","AEBODSYS","AEDECOD"), trtan_coln = 1),
    regexp = "Required columns missing in 'dmdata':",
    fixed  = FALSE
  )

  # BY variable not in indata
  testthat::expect_error(
    SOCbyPT(
      indata = indata,
      dmdata = dmdata,
      group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
      trtan_coln = 1,
      by_var = "SEX"
    ),
    regexp = "BY variable 'SEX' not found in 'indata'",
    fixed  = FALSE
  )
})

testthat::test_that("SOCbyPT: returns expected structure (no BY), includes TOTAL row, and TRT columns exist", {
  pick1 <- function(x) unname(x)[1]

  # deterministic small AE-like input
  indata <- data.frame(
    USUBJID   = c("S1","S1","S2","S3","S4","S4","S5"),
    TRTAN    = c(1,1,1,2,2,2,3),
    AEBODSYS  = c("SOC_A","SOC_A","SOC_B","SOC_A","SOC_C","SOC_C","SOC_A"),
    AEDECOD   = c("PT_1","PT_2","PT_1","PT_1","PT_1","UNCODED","PT_2"),
    stringsAsFactors = FALSE
  )

  # denominators
  dmdata <- data.frame(
    USUBJID = c("S1","S2","S3","S4","S5"),
    TRTAN  = c(1,1,2,2,3),
    stringsAsFactors = FALSE
  )

  out <- SOCbyPT(
    indata     = indata,
    dmdata     = dmdata,
    pop_data   = dmdata,
    group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln = 1,
    soc_totals = TRUE
  )

  testthat::expect_s3_class(out, "tbl_df")

  # Required columns: STAT, TRT* columns, sort_ord, sec_ord
  testthat::expect_true("STAT" %in% names(out))
  testthat::expect_true("sort_ord" %in% names(out))
  testthat::expect_true("sec_ord" %in% names(out))

  trt_cols <- grep("^TRT", names(out), value = TRUE)
  testthat::expect_true(length(trt_cols) >= 1)

  # TOTAL row exists at the top (sort_ord==0 & sec_ord==0)
  total_row <- out[out$sort_ord == 0 & out$sec_ord == 0, , drop = FALSE]
  testthat::expect_true(nrow(total_row) >= 1)
  testthat::expect_equal(pick1(total_row$STAT), "TOTAL SUBJECTS WITH AN EVENT")

  # TRT cells should be character with "n (pct)" or "0"
  # At minimum, ensure not all missing and that "0" replacement applied
  for (cc in trt_cols) {
    testthat::expect_true(is.character(out[[cc]]))
    testthat::expect_false(all(is.na(out[[cc]])))
  }
})

testthat::test_that("SOCbyPT: header_blank blanks SOC header rows (sort_ord==1) but not TOTAL row", {
  pick1 <- function(x) unname(x)[1]

  indata <- data.frame(
    USUBJID   = c("S1","S2","S3","S4"),
    TRTAN    = c(1,1,2,2),
    AEBODSYS  = c("SOC_A","SOC_A","SOC_A","SOC_B"),
    AEDECOD   = c("PT_1","PT_2","PT_1","PT_1"),
    stringsAsFactors = FALSE
  )

  dmdata <- data.frame(
    USUBJID = c("S1","S2","S3","S4"),
    TRTAN  = c(1,1,2,2),
    stringsAsFactors = FALSE
  )

  out <- SOCbyPT(
    indata        = indata,
    dmdata        = dmdata,
    pop_data      = dmdata,
    group_vars    = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln    = 1,
    header_blank  = TRUE,
    soc_totals    = TRUE
  )

  trt_cols <- grep("^TRT", names(out), value = TRUE)
  testthat::expect_true(length(trt_cols) >= 1)

  # TOTAL row should NOT be blanked
  total_row <- out[out$sort_ord == 0 & out$sec_ord == 0, , drop = FALSE]
  for (cc in trt_cols) {
    testthat::expect_false(is.na(pick1(total_row[[cc]])))
  }

  # SOC header rows (sort_ord==1) should be NA in TRT cols
  hdr <- out[out$sort_ord == 1, , drop = FALSE]
  testthat::expect_true(nrow(hdr) >= 1)
  for (cc in trt_cols) {
    # At least one header row treatment cell should be NA
    testthat::expect_true(any(is.na(hdr[[cc]])))
  }
})

testthat::test_that("SOCbyPT: uncoded_position='last' pushes UNCODED to bottom within each SOC block", {
  # Construct a SOC where PT includes UNCODED and another PT
  indata <- data.frame(
    USUBJID   = c("S1","S1","S2","S2"),
    TRTAN    = c(1,1,1,1),
    AEBODSYS  = c("SOC_A","SOC_A","SOC_A","SOC_A"),
    AEDECOD   = c("UNCODED","PT_1","UNCODED","PT_1"),
    stringsAsFactors = FALSE
  )

  dmdata <- data.frame(
    USUBJID = c("S1","S2"),
    TRTAN  = c(1,1),
    stringsAsFactors = FALSE
  )

  out <- SOCbyPT(
    indata           = indata,
    dmdata           = dmdata,
    pop_data         = dmdata,
    group_vars       = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln       = 1,
    rtf_safe         = FALSE,           # easier string checks
    soc_totals       = TRUE,
    uncoded_position = "last"
  )

  # find PT rows under SOC_A (sort_ord > 1); UNCODED should come after PT_1
  # Note: STAT has SOC header rows and PT rows; PT rows are sort_ord >= 2
  pt_rows <- out[out$STAT %in% c("PT_1","UNCODED"), c("STAT","sort_ord","sec_ord"), drop = FALSE]
  testthat::expect_equal(nrow(pt_rows), 2)

  ord <- pt_rows[order(pt_rows$sec_ord, pt_rows$sort_ord), "STAT", drop = TRUE]
  testthat::expect_identical(as.character(ord), c("PT_1","UNCODED"))
})

testthat::test_that("SOCbyPT: BY grouping adds BY columns and generates one TOTAL per BY group", {
  # AE with SEX as BY and a separate numeric sort key
  indata <- data.frame(
    USUBJID   = c("S1","S2","S3","S4","S5","S6"),
    TRTAN    = c(1,1,2,2,1,2),
    SEX      = c("F","F","F","M","M","M"),
    SEXN     = c(1,1,1,2,2,2),
    AEBODSYS  = c("SOC_A","SOC_A","SOC_A","SOC_A","SOC_B","SOC_B"),
    AEDECOD   = c("PT_1","PT_2","PT_1","PT_1","PT_1","UNCODED"),
    stringsAsFactors = FALSE
  )

  # dmdata includes SEX to allow bigN_by later if needed (here default is by trt only)
  dmdata <- unique(indata[c("USUBJID","TRTAN","SEX")])

  out <- SOCbyPT(
    indata          = indata,
    dmdata          = dmdata,
    pop_data        = dmdata,
    group_vars      = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln      = 1,
    by_var          = "SEX",
    by_sort_var     = "SEXN",
    by_sort_numeric = TRUE,
    soc_totals      = TRUE,
    rtf_safe        = FALSE
  )

  testthat::expect_true(all(c("SEX","SEXN") %in% names(out)))
  testthat::expect_true("STAT" %in% names(out))

  # TOTAL row should exist once per BY group
  tot <- out[out$STAT == "TOTAL SUBJECTS WITH AN EVENT" & out$sort_ord == 0 & out$sec_ord == 0, , drop = FALSE]
  testthat::expect_equal(sort(unique(tot$SEX)), c("F","M"))
})

testthat::test_that("SOCbyPT: bigN_by='YES' requires BY var in dmdata and changes denominators", {
  # AE with BY
  indata <- data.frame(
    USUBJID   = c("S1","S2","S3","S4"),
    TRTAN    = c(1,1,1,1),
    SEX      = c("F","F","M","M"),
    AEBODSYS  = c("SOC_A","SOC_A","SOC_A","SOC_A"),
    AEDECOD   = c("PT_1","PT_1","PT_1","PT_1"),
    stringsAsFactors = FALSE
  )

  # dmdata WITHOUT SEX -> should error when bigN_by="YES"
  dm_bad <- unique(indata[c("USUBJID","TRTAN")])
  testthat::expect_error(
    SOCbyPT(
      indata     = indata,
      dmdata     = dm_bad,
      pop_data   = dm_bad,
      group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
      trtan_coln = 1,
      by_var     = "SEX",
      bigN_by    = "YES"
    ),
    regexp = "bigN_by='YES' requires BY variable 'SEX' to exist in 'dmdata'",
    fixed  = FALSE
  )

  # dmdata WITH SEX -> should work
  dm_ok <- unique(indata[c("USUBJID","TRTAN","SEX")])

  out <- SOCbyPT(
    indata     = indata,
    dmdata     = dm_ok,
    pop_data   = dm_ok,
    group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln = 1,
    by_var     = "SEX",
    bigN_by    = "YES",
    rtf_safe   = FALSE
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true("SEX" %in% names(out))

  # With BY-specific denominators, TOTAL row is per BY group; verify both exist
  tot <- out[out$STAT == "TOTAL SUBJECTS WITH AN EVENT" & out$sort_ord == 0 & out$sec_ord == 0, , drop = FALSE]
  testthat::expect_equal(sort(unique(tot$SEX)), c("F","M"))
})

testthat::test_that("SOCbyPT: rtf_safe prefixes PT labels with indent_str", {
  indent <- "(*ESC*)R/RTF\"\\li360 \""

  indata <- data.frame(
    USUBJID   = c("S1","S2"),
    TRTAN    = c(1,1),
    AEBODSYS  = c("SOC_A","SOC_A"),
    AEDECOD   = c("PT_1","PT_2"),
    stringsAsFactors = FALSE
  )

  dmdata <- unique(indata[c("USUBJID","TRTAN")])

  out <- SOCbyPT(
    indata     = indata,
    dmdata     = dmdata,
    pop_data   = dmdata,
    group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln = 1,
    rtf_safe   = TRUE,
    indent_str = indent,
    soc_totals = TRUE
  )

  # PT rows have sort_ord >= 2; should contain indent prefix
  pt_rows <- out[out$sort_ord >= 2, , drop = FALSE]
  testthat::expect_true(nrow(pt_rows) >= 1)
  testthat::expect_true(any(startsWith(pt_rows$STAT, indent)))
})
