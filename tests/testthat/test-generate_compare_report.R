# tests/testthat/test-generate_compare_report.R

testthat::local_edition(3)

.write_rds <- function(dir, filename, x) saveRDS(x, file.path(dir, filename))
.write_csv <- function(dir, filename, x) utils::write.csv(x, file.path(dir, filename), row.names = FALSE, na = "")

# Robust coercion helpers (because compare CSV may store diffs as character)
.as_num1 <- function(x) {
  # returns numeric scalar or NA
  if (length(x) == 0) return(NA_real_)
  v <- suppressWarnings(as.numeric(as.character(x)[1]))
  v
}
.as_chr1 <- function(x) {
  if (length(x) == 0) return("")
  as.character(x)[1]
}

testthat::test_that("generate_compare_report: input validation (bad domain/dirs) errors", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_1"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_1"); dir.create(val_dir, showWarnings = FALSE)

  testthat::expect_error(
    generate_compare_report(domain = "", dev_dir = dev_dir, val_dir = val_dir),
    regexp = "domain must be a single, non-empty string",
    fixed  = FALSE
  )

  testthat::expect_error(
    generate_compare_report(domain = "adsl", dev_dir = "", val_dir = val_dir),
    regexp = "dev_dir must be a single, non-empty path string",
    fixed  = FALSE
  )

  testthat::expect_error(
    generate_compare_report(domain = "adsl", dev_dir = dev_dir, val_dir = "Z:/__nope__"),
    regexp = "VAL directory not found|not found",
    fixed  = FALSE
  )
})

testthat::test_that("generate_compare_report: resolves DEV domain case-insensitively and VAL prefix variants (v_, v-, v)", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_2"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_2"); dir.create(val_dir, showWarnings = FALSE)
  rpt_dir <- file.path(td, "rpt_gcr_2"); dir.create(rpt_dir, showWarnings = FALSE)

  dev <- data.frame(STUDYID = "S", USUBJID = c("01","02"), AVAL = c(1,2), stringsAsFactors = FALSE)
  val <- data.frame(STUDYID = "S", USUBJID = c("01","02"), AVAL = c(1,2), stringsAsFactors = FALSE)

  .write_csv(dev_dir, "adae.csv", dev)
  .write_csv(val_dir, "v-ADAE.csv", val)

  res <- generate_compare_report(
    domain        = "ADAE",
    dev_dir       = dev_dir,
    val_dir       = val_dir,
    by_vars       = c("STUDYID","USUBJID"),
    report_dir    = rpt_dir,
    prefix_val    = "v_",
    write_csv     = FALSE,
    run_comparedf = FALSE
  )

  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("only_in_dev","only_in_val","comparedf") %in% names(res)))
  testthat::expect_true(is.null(res$comparedf))
  testthat::expect_equal(nrow(res$only_in_dev), 0)
  testthat::expect_equal(nrow(res$only_in_val), 0)
})

testthat::test_that("generate_compare_report: ambiguous DEV match (multiple extensions) errors safely", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_3"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_3"); dir.create(val_dir, showWarnings = FALSE)

  df <- data.frame(STUDYID="S", USUBJID="01", AVAL=1, stringsAsFactors = FALSE)

  .write_csv(dev_dir, "adsl.csv", df)
  .write_rds(dev_dir, "adsl.rds", df)
  .write_csv(val_dir, "v_adsl.csv", df)

  testthat::expect_error(
    generate_compare_report(
      domain         = "adsl",
      dev_dir        = dev_dir,
      val_dir        = val_dir,
      by_vars        = c("STUDYID","USUBJID"),
      write_csv      = FALSE,
      run_comparedf  = FALSE
    ),
    regexp = "Ambiguous|Multiple matches|multiple",
    fixed  = FALSE
  )
})

testthat::test_that("generate_compare_report: missing VAL file errors with helpful diagnostics", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_4"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_4"); dir.create(val_dir, showWarnings = FALSE)

  df <- data.frame(STUDYID="S", USUBJID="01", AVAL=1, stringsAsFactors = FALSE)
  .write_csv(dev_dir, "adlb.csv", df)

  testthat::expect_error(
    generate_compare_report(
      domain        = "adlb",
      dev_dir       = dev_dir,
      val_dir       = val_dir,
      by_vars       = c("STUDYID","USUBJID"),
      write_csv     = FALSE,
      run_comparedf = FALSE
    ),
    regexp = "VAL dataset not found|Available files|not found",
    fixed  = FALSE
  )
})

testthat::test_that("generate_compare_report: write_csv creates PROC COMPARE-style CSV with _TYPE_/_OBS_ and numeric DIF (robust coercion)", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_5"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_5"); dir.create(val_dir, showWarnings = FALSE)
  rpt_dir <- file.path(td, "rpt_gcr_5"); dir.create(rpt_dir, showWarnings = FALSE)

  dev <- data.frame(
    STUDYID="S",
    USUBJID=c("01","02"),
    AVAL=c(10, 20),
    CHR = c("ABC", "AXC"),
    stringsAsFactors = FALSE
  )
  val <- data.frame(
    STUDYID="S",
    USUBJID=c("01","02"),
    AVAL=c(7, 25),
    CHR = c("ABC", "AYC"),
    stringsAsFactors = FALSE
  )

  .write_rds(dev_dir, "adae.rds", dev)
  .write_rds(val_dir, "v_adae.rds", val)

  generate_compare_report(
    domain        = "adae",
    dev_dir       = dev_dir,
    val_dir       = val_dir,
    by_vars       = c("STUDYID","USUBJID"),
    report_dir    = rpt_dir,
    write_csv     = TRUE,
    run_comparedf = FALSE
  )

  out_csv <- file.path(rpt_dir, "compare_adae.csv")
  testthat::expect_true(file.exists(out_csv))

  dt <- data.table::fread(out_csv)
  testthat::expect_true(all(c("_TYPE_","_OBS_","STUDYID","USUBJID") %in% names(dt)))
  testthat::expect_true(all(c("AVAL","CHR") %in% names(dt)))

  # Expect 2 keys * 3 rows each (BASE/COMPARE/DIF) = 6 rows
  testthat::expect_equal(nrow(dt), 6)
  testthat::expect_true(all(c("BASE","COMPARE","DIF") %in% unique(as.character(dt$`_TYPE_`))))

  dif_rows <- dt[as.character(dt$`_TYPE_`) == "DIF"]

  # USUBJID may be read numeric; compare as character after padding if needed
  usub <- as.character(dif_rows$USUBJID)

  dif_01 <- dif_rows[usub %in% c("01","1"), AVAL][1]
  dif_02 <- dif_rows[usub %in% c("02","2"), AVAL][1]

  testthat::expect_equal(.as_num1(dif_01), 3)
  testthat::expect_equal(.as_num1(dif_02), -5)

  # Character dif mask: allow any non-empty marker for differences
  chr_01 <- dif_rows[usub %in% c("01","1"), CHR][1]
  chr_02 <- dif_rows[usub %in% c("02","2"), CHR][1]

  # When equal, mask may be "" or all dots; when different, should be non-empty
  testthat::expect_true(nchar(.as_chr1(chr_02)) > 0)
})

testthat::test_that("generate_compare_report: Date/POSIXct DIF rows are produced (robust to CSV encoding)", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_6"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_6"); dir.create(val_dir, showWarnings = FALSE)
  rpt_dir <- file.path(td, "rpt_gcr_6"); dir.create(rpt_dir, showWarnings = FALSE)

  dev <- data.frame(
    STUDYID = "S",
    USUBJID = c("01","02"),
    ADT     = as.Date(c("2020-01-02","2020-01-10")),
    ADTM    = as.POSIXct(c("2020-01-02 00:00:10","2020-01-10 00:00:00"), tz="UTC"),
    stringsAsFactors = FALSE
  )
  val <- data.frame(
    STUDYID = "S",
    USUBJID = c("01","02"),
    ADT     = as.Date(c("2020-01-01","2020-01-12")),
    ADTM    = as.POSIXct(c("2020-01-02 00:00:00","2020-01-10 00:00:30"), tz="UTC"),
    stringsAsFactors = FALSE
  )

  .write_rds(dev_dir, "adsl.rds", dev)
  .write_rds(val_dir, "v_adsl.rds", val)

  generate_compare_report(
    domain        = "adsl",
    dev_dir       = dev_dir,
    val_dir       = val_dir,
    by_vars       = c("STUDYID","USUBJID"),
    report_dir    = rpt_dir,
    write_csv     = TRUE,
    run_comparedf = FALSE
  )

  dt  <- data.table::fread(file.path(rpt_dir, "compare_adsl.csv"))
  testthat::expect_true(all(c("ADT","ADTM","USUBJID","_TYPE_") %in% names(dt)))

  dif <- dt[as.character(dt$`_TYPE_`) == "DIF"]
  testthat::expect_equal(nrow(dif), 2)

  # USUBJID may be read as 1/2 or "01"/"02"
  got_ids <- sort(unique(as.character(dif$USUBJID)))
  testthat::expect_true(all(got_ids %in% c("01","02","1","2")))

  # Values may not be numeric in CSV; ensure they are not both blank across rows
  adt_vals  <- as.character(dif$ADT)
  adtm_vals <- as.character(dif$ADTM)

  testthat::expect_true(any(nzchar(adt_vals[!is.na(adt_vals)])) || any(!is.na(dif$ADT)))
  testthat::expect_true(any(nzchar(adtm_vals[!is.na(adtm_vals)])) || any(!is.na(dif$ADTM)))

  # Optional: if numeric coercion works in this environment, assert expected diffs
  adt_num  <- suppressWarnings(as.numeric(adt_vals))
  adtm_num <- suppressWarnings(as.numeric(adtm_vals))

  if (any(!is.na(adt_num))) {
    # expected: 1 and -2 (order may vary)
    testthat::expect_true(all(sort(adt_num[!is.na(adt_num)]) == c(-2, 1)))
  }
  if (any(!is.na(adtm_num))) {
    # expected: 10 and -30
    testthat::expect_true(all(sort(adtm_num[!is.na(adtm_num)]) == c(-30, 10)))
  }
})


testthat::test_that("generate_compare_report: filter_expr reduces keys (robust to USUBJID read as numeric)", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_7"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_7"); dir.create(val_dir, showWarnings = FALSE)
  rpt_dir <- file.path(td, "rpt_gcr_7"); dir.create(rpt_dir, showWarnings = FALSE)

  dev <- data.frame(
    STUDYID="S",
    USUBJID=c("01","02","03"),
    SAFFL=c("Y","N","Y"),
    AVAL=c(1,2,3),
    stringsAsFactors = FALSE
  )
  val <- data.frame(
    STUDYID="S",
    USUBJID=c("01","02","03"),
    SAFFL=c("Y","N","Y"),
    AVAL=c(1,999,3),
    stringsAsFactors = FALSE
  )

  .write_rds(dev_dir, "adae.rds", dev)
  .write_rds(val_dir, "v_adae.rds", val)

  generate_compare_report(
    domain        = "adae",
    dev_dir       = dev_dir,
    val_dir       = val_dir,
    by_vars       = c("STUDYID","USUBJID"),
    report_dir    = rpt_dir,
    filter_expr   = "SAFFL == 'Y'",
    write_csv     = TRUE,
    run_comparedf = FALSE
  )

  dt <- data.table::fread(file.path(rpt_dir, "compare_adae.csv"))

  # USUBJID may be numeric (1,3) or char ("01","03")
  got <- unique(as.character(dt$USUBJID))
  testthat::expect_true(all(got %in% c("01","03","1","3")))
  testthat::expect_false(any(got %in% c("02","2")))
})

testthat::test_that("generate_compare_report: run_comparedf=TRUE writes .lst report (skip if arsenal missing)", {
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("haven")
  testthat::skip_if_not_installed("arsenal")

  td <- tempdir()
  dev_dir <- file.path(td, "dev_gcr_8"); dir.create(dev_dir, showWarnings = FALSE)
  val_dir <- file.path(td, "val_gcr_8"); dir.create(val_dir, showWarnings = FALSE)
  rpt_dir <- file.path(td, "rpt_gcr_8"); dir.create(rpt_dir, showWarnings = FALSE)

  dev <- data.frame(STUDYID="S", USUBJID=c("01","02"), AVAL=c(1,2), stringsAsFactors = FALSE)
  val <- data.frame(STUDYID="S", USUBJID=c("01","02"), AVAL=c(1,3), stringsAsFactors = FALSE)

  .write_rds(dev_dir, "adsl.rds", dev)
  .write_rds(val_dir, "v_adsl.rds", val)

  generate_compare_report(
    domain        = "adsl",
    dev_dir       = dev_dir,
    val_dir       = val_dir,
    by_vars       = c("STUDYID","USUBJID"),
    report_dir    = rpt_dir,
    prefix_val    = "v_",
    write_csv     = FALSE,
    run_comparedf = TRUE,
    study_id      = "TESTSTUDY",
    author        = "UnitTest"
  )

  lst <- file.path(rpt_dir, "v_adsl.lst")
  testthat::expect_true(file.exists(lst))

  txt <- readLines(lst, warn = FALSE)
  testthat::expect_true(any(grepl("R Comparison Report", txt)))
  testthat::expect_true(any(grepl("TESTSTUDY", txt)))
  testthat::expect_true(any(grepl("UnitTest", txt)))
})
