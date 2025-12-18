# tests/testthat/test-ATCbyDrug.R

# Helper: safe “is UNCODED” check that treats NA as FALSE
.is_uncoded <- function(x) {
  !is.na(x) & grepl("^\\s*UNCODED\\s*$", x, ignore.case = TRUE)
}

testthat::test_that("ATCbyDrug: returns expected structure and fills missing trt cells with '0'", {
  dm <- data.frame(
    USUBJID = c("S1","S2","S3","S4"),
    TRTAN   = c(21,   21,  22,  22),
    stringsAsFactors = FALSE
  )

  # Two records for S1 same drug => should count once (distinct subject)
  indata <- data.frame(
    USUBJID = c("S1","S1","S2","S3","S4"),
    TRTAN   = c(21,  21,  21,  22,  22),
    ATC2    = c("A2","A2","A2","B2","UNCODED"),
    ATC4    = c("A4A","A4A","A4B","B4A","UNCODED"),
    CMDECOD = c("DRUG1","DRUG1","DRUG2","DRUG3","UNCODED"),
    stringsAsFactors = FALSE
  )

  out <- ATCbyDrug(
    indata      = indata,
    dmdata      = dm,
    group_vars  = c("TRTAN","ATC2","ATC4","CMDECOD"),
    trtan_coln  = "21",
    rtf_safe    = TRUE,
    sort_by     = "count"
  )

  testthat::expect_s3_class(out, "tbl_df")

  # Required ordering/display cols
  testthat::expect_true(all(c("stat","sec_ord","psec_ord","sort_ord") %in% names(out)))

  # Treatment columns exist after rename result_* -> trt*
  testthat::expect_true(all(c("trt21","trt22") %in% names(out)))

  # Raw count columns exist
  testthat::expect_true(all(c("n__21","n__22") %in% names(out)))

  # NA result cells should be filled to "0" (this checks the “result_” fill worked)
  testthat::expect_false(any(is.na(out$trt21)))
  testthat::expect_false(any(is.na(out$trt22)))

  # A2 top-level row exists: ATC2 == "A2" and ATC4/CMDECOD missing
  a2_row <- out[!is.na(out$ATC2) & out$ATC2 == "A2" & is.na(out$ATC4) & is.na(out$CMDECOD), , drop = FALSE]
  testthat::expect_equal(nrow(a2_row), 1)
  # A2 only in TRT=21, so TRT=22 should be "0"
  testthat::expect_equal(a2_row$trt22[[1]], "0")

  # Drug-level row: must have ATC2/ATC4/CMDECOD all present (prevents grabbing parent rows)
  d1_rows <- out[
    !is.na(out$ATC2) & out$ATC2 == "A2" &
      !is.na(out$ATC4) & out$ATC4 == "A4A" &
      !is.na(out$CMDECOD) & out$CMDECOD == "DRUG1",
    , drop = FALSE
  ]

  # In correct output, this should be exactly 1 row
  testthat::expect_equal(nrow(d1_rows), 1)

  # Distinct-subject check: DRUG1 in TRT=21 is only S1 => n=1, denom for TRT=21 is 2 => 50.0
  testthat::expect_equal(d1_rows$trt21[[1]], "1 (50.0)")
})

testthat::test_that("ATCbyDrug: RTF indent mode applies to ATC4 and CMDECOD rows (when spaces are NULL)", {
  dm <- data.frame(USUBJID=c("S1","S2","S3","S4"), TRTAN=c(21,21,22,22), stringsAsFactors=FALSE)
  indata <- data.frame(
    USUBJID=c("S1","S2","S3","S4"),
    TRTAN=c(21,21,22,22),
    ATC2=c("A2","A2","B2","B2"),
    ATC4=c("A4A","A4B","B4A","B4A"),
    CMDECOD=c("DRUG1","DRUG2","DRUG3","DRUG4"),
    stringsAsFactors=FALSE
  )

  out <- ATCbyDrug(
    indata     = indata,
    dmdata     = dm,
    group_vars = c("TRTAN","ATC2","ATC4","CMDECOD"),
    trtan_coln = "21",
    rtf_safe   = TRUE,
    sort_by    = "alpha"
  )

  # ATC4-level rows: ATC4 present, CMDECOD NA
  atc4_rows <- out[!is.na(out$ATC4) & is.na(out$CMDECOD), , drop = FALSE]
  testthat::expect_true(nrow(atc4_rows) > 0)
  testthat::expect_true(all(grepl("^\\(\\*ESC\\*\\)R/RTF", atc4_rows$stat)))

  # Drug rows: CMDECOD present
  med_rows <- out[!is.na(out$CMDECOD), , drop = FALSE]
  testthat::expect_true(nrow(med_rows) > 0)
  testthat::expect_true(all(grepl("^\\(\\*ESC\\*\\)R/RTF", med_rows$stat)))

  # ATC2 rows: ATC4 and CMDECOD are NA => no RTF prefix
  atc2_rows <- out[is.na(out$ATC4) & is.na(out$CMDECOD), , drop = FALSE]
  testthat::expect_true(nrow(atc2_rows) > 0)
  testthat::expect_false(any(grepl("^\\(\\*ESC\\*\\)R/RTF", atc2_rows$stat)))
})

testthat::test_that("ATCbyDrug: spaces mode uses blank indents (no RTF) when atc4_spaces/cmdecod_spaces provided", {
  dm <- data.frame(USUBJID=c("S1","S2","S3","S4"), TRTAN=c(21,21,22,22), stringsAsFactors=FALSE)
  indata <- data.frame(
    USUBJID=c("S1","S2","S3","S4"),
    TRTAN=c(21,21,22,22),
    ATC2=c("A2","A2","B2","B2"),
    ATC4=c("A4A","A4B","B4A","B4A"),
    CMDECOD=c("DRUG1","DRUG2","DRUG3","DRUG4"),
    stringsAsFactors=FALSE
  )

  out <- ATCbyDrug(
    indata     = indata,
    dmdata     = dm,
    group_vars = c("TRTAN","ATC2","ATC4","CMDECOD"),
    trtan_coln = "21",
    sort_by    = "alpha",
    atc4_spaces    = 2,
    cmdecod_spaces = 4
  )

  atc4_rows <- out[!is.na(out$ATC4) & is.na(out$CMDECOD), , drop = FALSE]
  med_rows  <- out[!is.na(out$CMDECOD), , drop = FALSE]

  testthat::expect_true(all(grepl("^  \\S", atc4_rows$stat)))     # 2 spaces
  testthat::expect_true(all(grepl("^    \\S", med_rows$stat)))    # 4 spaces

  testthat::expect_false(any(grepl("^\\(\\*ESC\\*\\)R/RTF", c(atc4_rows$stat, med_rows$stat))))
})

testthat::test_that("ATCbyDrug: sort_by='alpha' orders ATC4 within ATC2 alphabetically, and drugs within ATC4 alphabetically", {
  dm <- data.frame(
    USUBJID=c("S1","S2","S3","S4","S5","S6"),
    TRTAN  =c(21,  21,  21,  22,  22,  22),
    stringsAsFactors=FALSE
  )

  indata <- data.frame(
    USUBJID=c("S1","S2","S3","S4","S5","S6"),
    TRTAN  =c(21,  21,  21,  22,  22,  22),
    ATC2   =c("A2","A2","A2","A2","A2","A2"),
    ATC4   =c("Z4","A4","M4","Z4","A4","M4"),
    CMDECOD=c("ZDRUG","ADRUG","MDRUG","ZDRUG","ADRUG","MDRUG"),
    stringsAsFactors=FALSE
  )

  out <- ATCbyDrug(
    indata     = indata,
    dmdata     = dm,
    group_vars = c("TRTAN","ATC2","ATC4","CMDECOD"),
    trtan_coln = "21",
    sort_by    = "alpha",
    rtf_safe   = FALSE
  )

  # ATC4 rows in display order
  atc4_rows <- out[out$ATC2 == "A2" & !is.na(out$ATC4) & is.na(out$CMDECOD), , drop=FALSE]
  testthat::expect_true(nrow(atc4_rows) >= 3)
  testthat::expect_identical(as.character(atc4_rows$ATC4), c("A4","M4","Z4"))

  # Drug rows order should follow ADRUG, MDRUG, ZDRUG
  med_rows <- out[out$ATC2 == "A2" & !is.na(out$CMDECOD), , drop=FALSE]
  testthat::expect_true(nrow(med_rows) >= 3)
  testthat::expect_identical(as.character(med_rows$CMDECOD), c("ADRUG","MDRUG","ZDRUG"))
})

testthat::test_that("ATCbyDrug: fully-UNCODED row is placed at the end (ATC2=ATC4=CMDECOD='UNCODED')", {
  dm <- data.frame(USUBJID=c("S1","S2","S3","S4"), TRTAN=c(21,21,22,22), stringsAsFactors=FALSE)

  indata <- data.frame(
    USUBJID=c("S1","S2","S3","S4"),
    TRTAN  =c(21,  21,  22,  22),
    ATC2   =c("A2","A2","B2","UNCODED"),
    ATC4   =c("A4A","A4B","B4A","UNCODED"),
    CMDECOD=c("DRUG1","DRUG2","DRUG3","UNCODED"),
    stringsAsFactors=FALSE
  )

  out <- ATCbyDrug(
    indata     = indata,
    dmdata     = dm,
    group_vars = c("TRTAN","ATC2","ATC4","CMDECOD"),
    trtan_coln = "21",
    sort_by    = "alpha",
    rtf_safe   = FALSE
  )

  # The last row should be the fully-UNCODED drug row (robust to NA propagation)
  last <- out[nrow(out), , drop = FALSE]

  testthat::expect_true(.is_uncoded(last$ATC2[[1]]))
  testthat::expect_true(.is_uncoded(last$ATC4[[1]]))
  testthat::expect_true(.is_uncoded(last$CMDECOD[[1]]))
})
