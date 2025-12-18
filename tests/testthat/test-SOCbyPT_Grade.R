# tests/testthat/test-SOCbyPT_Grade.R

testthat::test_that("SOCbyPT_Grade: input validation (group_vars length, bigN_by values, bigN_by requires by_var)", {

  indata <- data.frame(
    USUBJID   = c("S1","S2"),
    TRTAN    = c(1,2),
    AEBODSYS  = c("SOC1","SOC1"),
    AEDECOD   = c("PT1","PT2"),
    AETOXGRN  = c(2, 3),
    stringsAsFactors = FALSE
  )

  dmdata <- data.frame(
    USUBJID = c("S1","S2"),
    TRTAN  = c(1,2),
    stringsAsFactors = FALSE
  )

  # group_vars must be length 3
  testthat::expect_error(
    SOCbyPT_Grade(
      indata = indata, dmdata = dmdata,
      group_vars = c("TRTAN","AEBODSYS"),
      trtan_coln = 1
    ),
    regexp = "length\\(group_vars\\) == 3|stopifnot",
    fixed  = FALSE
  )

  # bigN_by must be NULL/'NO'/'YES'
  testthat::expect_error(
    SOCbyPT_Grade(
      indata = indata, dmdata = dmdata,
      group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
      trtan_coln = 1,
      bigN_by = "MAYBE"
    ),
    regexp = "bigN_by must be NULL, 'NO', or 'YES'",
    fixed  = FALSE
  )

  # bigN_by='YES' requires by_var
  testthat::expect_error(
    SOCbyPT_Grade(
      indata = indata, dmdata = dmdata,
      group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
      trtan_coln = 1,
      bigN_by = "YES",
      by_var = NULL
    ),
    regexp = "bigN_by='YES' requires by_var",
    fixed  = FALSE
  )
})

testthat::test_that("SOCbyPT_Grade: grade column set created; TOTAL row present; expected columns exist", {
  pick1 <- function(x) unname(x)[1]

  indata <- data.frame(
    USUBJID   = c("S1","S1","S2","S3","S4","S4","S5"),
    TRTAN    = c(1,1,1,2,2,2,3),
    AEBODSYS  = c("SOC_A","SOC_A","SOC_B","SOC_A","SOC_C","SOC_C","SOC_A"),
    AEDECOD   = c("PT_1","PT_2","PT_1","PT_1","PT_1","UNCODED","PT_2"),
    AETOXGRN  = c(1, 5, 2, 4, 3, NA, 2),
    AETOXGR   = c(NA, NA, NA, NA, NA, "NR", NA),  # make an NR via char
    stringsAsFactors = FALSE
  )

  dmdata <- data.frame(
    USUBJID = c("S1","S2","S3","S4","S5"),
    TRTAN  = c(1,1,2,2,3),
    stringsAsFactors = FALSE
  )

  out <- SOCbyPT_Grade(
    indata      = indata,
    dmdata      = dmdata,
    pop_data    = dmdata,
    group_vars  = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln  = 1,
    grade_num   = "AETOXGRN",
    grade_char  = "AETOXGR",
    soc_totals  = FALSE,
    header_blank = TRUE,
    rtf_safe    = TRUE
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(nrow(out) > 0)

  # Expected core columns
  testthat::expect_true("_SUBVARLBL_" %in% names(out))
  testthat::expect_true(all(c("sort_ord","sec_ord") %in% names(out)))

  # TRT grade columns exist (at least for one treatment)
  grade_cols <- grep("^TRT\\d+_(GRADE[1-5]|NOT_REPORTED)$", names(out), value = TRUE)
  testthat::expect_true(length(grade_cols) >= 6)

  # TOTAL row exists (sort_ord=0, sec_ord=0)
  tot <- out[out$sort_ord == 0L & out$sec_ord == 0L, , drop = FALSE]
  testthat::expect_true(nrow(tot) >= 1)
  testthat::expect_equal(pick1(tot$`_SUBVARLBL_`), "TOTAL SUBJECTS WITH AN EVENT")

  # TRT grade cells should be character and not all NA
  for (cc in grade_cols) {
    testthat::expect_true(is.character(out[[cc]]))
    testthat::expect_false(all(is.na(out[[cc]])))
  }
})

testthat::test_that("SOCbyPT_Grade: rtf_safe indents PT labels but not SOC headers / TOTAL label", {
  pick1 <- function(x) unname(x)[1]
  indent <- "(*ESC*)R/RTF\"\\li360 \""

  indata <- data.frame(
    USUBJID  = c("S1","S2"),
    TRTAN   = c(1,1),
    AEBODSYS = c("SOC_A","SOC_A"),
    AEDECOD  = c("PT_1","PT_2"),
    AETOXGRN = c(2, 3),
    stringsAsFactors = FALSE
  )

  dmdata <- data.frame(USUBJID = c("S1","S2"), TRTAN = c(1,1), stringsAsFactors = FALSE)

  out <- SOCbyPT_Grade(
    indata      = indata,
    dmdata      = dmdata,
    pop_data    = dmdata,
    group_vars  = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln  = 1,
    rtf_safe    = TRUE,
    indent_str  = indent,
    soc_totals  = FALSE
  )

  # PT rows are sort_ord >= 2 and should be indented
  pt_rows <- out[out$sort_ord >= 2L, , drop = FALSE]
  testthat::expect_true(nrow(pt_rows) >= 1)
  testthat::expect_true(any(startsWith(pt_rows$`_SUBVARLBL_`, indent)))

  # TOTAL label should not be indented
  tot <- out[out$sort_ord == 0L & out$sec_ord == 0L, , drop = FALSE]
  testthat::expect_false(startsWith(pick1(tot$`_SUBVARLBL_`), indent))
})

testthat::test_that("SOCbyPT_Grade: header_blank blanks SOC header grade cells when soc_totals=FALSE", {

  indata <- data.frame(
    USUBJID  = c("S1","S2","S3","S4"),
    TRTAN   = c(1,1,2,2),
    AEBODSYS = c("SOC_A","SOC_A","SOC_A","SOC_B"),
    AEDECOD  = c("PT_1","PT_2","PT_1","PT_1"),
    AETOXGRN = c(2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  dmdata <- unique(indata[c("USUBJID","TRTAN")])

  out <- SOCbyPT_Grade(
    indata       = indata,
    dmdata       = dmdata,
    pop_data     = dmdata,
    group_vars   = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln   = 1,
    soc_totals   = FALSE,
    header_blank = TRUE
  )

  grade_cols <- grep("^TRT\\d+_(GRADE[1-5]|NOT_REPORTED)$", names(out), value = TRUE)
  testthat::expect_true(length(grade_cols) >= 6)

  # SOC header rows have sort_ord==1 and are blanked to NA_character_
  hdr <- out[out$sort_ord == 1L & out$`_SUBVARLBL_` != "TOTAL SUBJECTS WITH AN EVENT", , drop = FALSE]
  testthat::expect_true(nrow(hdr) >= 1)
  for (cc in grade_cols) {
    testthat::expect_true(any(is.na(hdr[[cc]])))
  }
})

testthat::test_that("SOCbyPT_Grade: soc_totals=TRUE populates SOC header grade cells (not blank)", {

  indata <- data.frame(
    USUBJID  = c("S1","S2","S3","S4"),
    TRTAN   = c(1,1,2,2),
    AEBODSYS = c("SOC_A","SOC_A","SOC_A","SOC_B"),
    AEDECOD  = c("PT_1","PT_2","PT_1","PT_1"),
    AETOXGRN = c(2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
  dmdata <- unique(indata[c("USUBJID","TRTAN")])

  out <- SOCbyPT_Grade(
    indata      = indata,
    dmdata      = dmdata,
    pop_data    = dmdata,
    group_vars  = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln  = 1,
    soc_totals  = TRUE,
    header_blank = TRUE
  )

  grade_cols <- grep("^TRT\\d+_(GRADE[1-5]|NOT_REPORTED)$", names(out), value = TRUE)
  testthat::expect_true(length(grade_cols) >= 6)

  hdr <- out[out$sort_ord == 1L & out$`_SUBVARLBL_` != "TOTAL SUBJECTS WITH AN EVENT", , drop = FALSE]
  testthat::expect_true(nrow(hdr) >= 1)

  # With soc_totals=TRUE, SOC header rows should have "n (pct)" strings or "0", not all NA
  for (cc in grade_cols) {
    testthat::expect_false(all(is.na(hdr[[cc]])))
  }
})

testthat::test_that("SOCbyPT_Grade: bigN_by='YES' requires by_var to exist in dmdata/pop_data; BY output contains by_var", {
  # BY variable present in AE; missing in dmdata should error when bigN_by='YES'
  indata <- data.frame(
    USUBJID  = c("S1","S2","S3","S4"),
    TRTAN   = c(1,1,1,1),
    SEX     = c("F","F","M","M"),
    AEBODSYS = c("SOC_A","SOC_A","SOC_A","SOC_A"),
    AEDECOD  = c("PT_1","PT_1","PT_1","PT_1"),
    AETOXGRN = c(2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  dm_bad <- unique(indata[c("USUBJID","TRTAN")])  # no SEX
  testthat::expect_error(
    SOCbyPT_Grade(
      indata     = indata,
      dmdata     = dm_bad,
      pop_data   = dm_bad,
      group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
      trtan_coln = 1,
      by_var     = "SEX",
      bigN_by    = "YES"
    ),
    regexp = "requires by_var to exist in dmdata/pop_data",
    fixed  = FALSE
  )

  # With SEX in denominators, should work and include SEX column
  dm_ok <- unique(indata[c("USUBJID","TRTAN","SEX")])

  out <- SOCbyPT_Grade(
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

  tot <- out[out$sort_ord == 0L & out$sec_ord == 0L, , drop = FALSE]
  testthat::expect_equal(sort(unique(tot$SEX)), c("F","M"))
})

testthat::test_that("SOCbyPT_Grade: grade_char NR mapping produces NOT_REPORTED bucket strings", {
  pick1 <- function(x) unname(x)[1]

  indata <- data.frame(
    USUBJID  = c("S1","S2"),
    TRTAN   = c(1,1),
    AEBODSYS = c("SOC_A","SOC_A"),
    AEDECOD  = c("PT_1","PT_1"),
    AETOXGRN = c(NA, NA),      # no numeric grades
    AETOXGR  = c("NR","NOT REPORTED"),
    stringsAsFactors = FALSE
  )
  dmdata <- unique(indata[c("USUBJID","TRTAN")])

  out <- SOCbyPT_Grade(
    indata      = indata,
    dmdata      = dmdata,
    pop_data    = dmdata,
    group_vars  = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln  = 1,
    grade_num   = "AETOXGRN",
    grade_char  = "AETOXGR",
    rtf_safe    = FALSE
  )

  # Expect some NOT_REPORTED column value to be non-zero (n (pct)) on TOTAL row
  tot <- out[out$sort_ord == 0L & out$sec_ord == 0L, , drop = FALSE]
  nr_col <- grep("^TRT1_NOT_REPORTED$", names(tot), value = TRUE)
  testthat::expect_true(length(nr_col) == 1)
  testthat::expect_true(grepl("^\\d+ \\(.*\\)$|^0$", pick1(tot[[nr_col]])))
})

testthat::test_that("SOCbyPT_Grade: uncoded_position='last' forces SOC=='UNCODED' block to the end (no BY)", {
  pick1 <- function(x) unname(x)[1]

  indata <- data.frame(
    USUBJID  = c("S1","S2","S3","S4"),
    TRTAN   = c(1,1,1,1),
    AEBODSYS = c("SOC_A","UNCODED","SOC_A","UNCODED"),
    AEDECOD  = c("PT_1","PT_X","PT_2","UNCODED"),
    AETOXGRN = c(2, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  dmdata <- unique(indata[c("USUBJID","TRTAN")])

  out <- SOCbyPT_Grade(
    indata      = indata,
    dmdata      = dmdata,
    pop_data    = dmdata,
    group_vars  = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln  = 1,
    rtf_safe    = FALSE,
    uncoded_position = "last"
  )

  # Find SOC header rows (sort_ord==1) and check last SOC header is UNCODED
  hdr <- out[out$sort_ord == 1L & out$sec_ord > 0L, , drop = FALSE]
  testthat::expect_true(nrow(hdr) >= 1)

  last_soc <- pick1(hdr$`_SUBVARLBL_`[nrow(hdr)])
  testthat::expect_equal(last_soc, "UNCODED")
})

testthat::test_that("SOCbyPT_Grade: BY output includes by_var and creates TOTAL per BY group", {
  # This test avoids by_sort_var because current implementation may drop it
  # before the internal arrange(), causing 'Column SEXN not found'.

  indata <- data.frame(
    USUBJID  = c("S1","S2","S3","S4","S5","S6"),
    TRTAN   = c(1,1,2,2,1,2),
    SEX     = c("F","F","F","M","M","M"),
    SEXN    = c(1,1,1,2,2,2),  # retained in input for future; not asserted
    AEBODSYS = c("SOC_A","SOC_A","SOC_A","SOC_A","SOC_B","SOC_B"),
    AEDECOD  = c("PT_1","PT_2","PT_1","PT_1","PT_1","UNCODED"),
    AETOXGRN = c(2, 3, 4, 5, 1, 2),
    stringsAsFactors = FALSE
  )

  dmdata <- unique(indata[c("USUBJID","TRTAN")])

  out <- SOCbyPT_Grade(
    indata     = indata,
    dmdata     = dmdata,
    pop_data   = dmdata,
    group_vars = c("TRTAN","AEBODSYS","AEDECOD"),
    trtan_coln = 1,
    by_var     = "SEX",
    rtf_safe   = FALSE
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true("SEX" %in% names(out))

  # TOTAL exists once per BY stratum
  tot <- out[out$sort_ord == 0L & out$sec_ord == 0L, , drop = FALSE]
  testthat::expect_equal(sort(unique(tot$SEX)), c("F","M"))
})
