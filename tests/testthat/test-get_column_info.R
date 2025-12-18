testthat::test_that("get_column_info: errors if df is not a data.frame", {
  testthat::expect_error(
    get_column_info(1),
    regexp = "`df` must be a data.frame|data.frame or tibble",
    fixed  = FALSE
  )

  testthat::expect_error(
    get_column_info("abc"),
    regexp = "`df` must be a data.frame|data.frame or tibble",
    fixed  = FALSE
  )
})

testthat::test_that("get_column_info: returns expected tibble structure for mixed types", {
  # Helper: safely pick a single scalar from a subset while dropping names
  pick1 <- function(x) unname(x)[1]

  df <- data.frame(
    A = c(1, 2, NA, 4),
    B = c("x", NA, "y", "y"),
    C = as.Date(c("2012-07-09", NA, "2012-07-11", "2012-07-10")),
    D = as.POSIXct(
      c("2012-07-09 10:00:00", NA, "2012-07-09 12:00:00", "2012-07-09 11:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  # Add label/format attributes (SAS-like)
  attr(df$A, "label")      <- "Numeric A"
  attr(df$A, "format.sas") <- "BEST12."
  attr(df$C, "label")      <- "A Date"
  attr(df$C, "format")     <- "DATE9."
  attr(df$D, "Label")      <- "A Datetime"
  attr(df$D, "Format")     <- "DATETIME20."

  out <- get_column_info(df)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), ncol(df))

  # Required columns
  req <- c(
    "column", "label", "format", "class", "typeof",
    "n", "n_missing", "n_unique",
    "min_raw", "max_raw", "min_disp", "max_disp",
    "sample_disp", "attribute_names", "attributes"
  )
  testthat::expect_true(all(req %in% names(out)))

  # Column names captured
  testthat::expect_equal(out$column, names(df))

  # n, n_missing, n_unique are character (by design in your function)
  testthat::expect_equal(pick1(out$n[out$column == "A"]), "4")
  testthat::expect_equal(pick1(out$n_missing[out$column == "A"]), "1")
  testthat::expect_equal(
    pick1(out$n_unique[out$column == "B"]),
    as.character(length(unique(df$B)))
  )

  # Label/format pulled using priority vectors
  testthat::expect_equal(pick1(out$label[out$column == "A"]), "Numeric A")
  testthat::expect_equal(pick1(out$format[out$column == "A"]), "BEST12.")
  testthat::expect_equal(pick1(out$label[out$column == "C"]), "A Date")
  testthat::expect_equal(pick1(out$format[out$column == "C"]), "DATE9.")
  testthat::expect_equal(pick1(out$label[out$column == "D"]), "A Datetime")
  testthat::expect_equal(pick1(out$format[out$column == "D"]), "DATETIME20.")
})

testthat::test_that("get_column_info: range computation and SAS-like display for Date/POSIXct works", {
  pick1 <- function(x) unname(x)[1]

  df <- data.frame(
    A = c(10, 5, NA, 20),
    C = as.Date(c("2012-07-09", NA, "2012-07-11", "2012-07-10")),
    D = as.POSIXct(
      c("2012-07-09 10:00:00", NA, "2012-07-09 12:00:00", "2012-07-09 11:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  out <- get_column_info(df, compute_ranges = TRUE, sas_date_display = TRUE)

  # Numeric min/max display
  testthat::expect_equal(pick1(out$min_disp[out$column == "A"]), "5")
  testthat::expect_equal(pick1(out$max_disp[out$column == "A"]), "20")

  # Date min/max display (DATE9 style)
  testthat::expect_equal(pick1(out$min_disp[out$column == "C"]), "09JUL2012")
  testthat::expect_equal(pick1(out$max_disp[out$column == "C"]), "11JUL2012")

  # POSIXct min/max display: "%d%b%Y:%H:%M:%S" uppercased
  testthat::expect_match(pick1(out$min_disp[out$column == "D"]), "^09JUL2012:10:00:00$")
  testthat::expect_match(pick1(out$max_disp[out$column == "D"]), "^09JUL2012:12:00:00$")

  # sample_disp returns first non-missing display for Date/POSIXct only
  testthat::expect_equal(pick1(out$sample_disp[out$column == "C"]), "09JUL2012")
  testthat::expect_match(pick1(out$sample_disp[out$column == "D"]), "^09JUL2012:10:00:00$")
  testthat::expect_equal(pick1(out$sample_disp[out$column == "A"]), "")
})

testthat::test_that("get_column_info: compute_ranges=FALSE blanks out range/display fields", {
  pick1 <- function(x) unname(x)[1]

  df <- data.frame(
    A = c(1, 2, NA),
    C = as.Date(c("2012-07-09", NA, "2012-07-11")),
    stringsAsFactors = FALSE
  )

  out <- get_column_info(df, compute_ranges = FALSE, sas_date_display = TRUE)

  # min_raw/max_raw are list-cols: entries should be NA
  idxA <- which(out$column == "A")[1]
  testthat::expect_true(is.na(out$min_raw[[idxA]]))
  testthat::expect_true(is.na(out$max_raw[[idxA]]))

  # display strings should be blank
  testthat::expect_equal(pick1(out$min_disp[out$column == "A"]), "")
  testthat::expect_equal(pick1(out$max_disp[out$column == "A"]), "")
})

testthat::test_that("get_column_info: include_attributes=FALSE drops attributes list-column", {
  df <- data.frame(A = 1:3)

  out <- get_column_info(df, include_attributes = FALSE)

  testthat::expect_false("attributes" %in% names(out))
  testthat::expect_true("attribute_names" %in% names(out))
})

testthat::test_that("get_column_info: exclude_attributes removes unwanted attribute names", {
  df <- data.frame(A = 1:3)
  attr(df$A, "label")     <- "My Label"
  attr(df$A, "format")    <- "BEST12."
  attr(df$A, "my_custom") <- "X"

  out <- get_column_info(
    df,
    include_attributes = TRUE,
    exclude_attributes = c("class", "row.names", "label")  # drop label
  )

  nm <- out$attribute_names[out$column == "A"]
  nm <- unname(nm)[1]

  testthat::expect_false(grepl("\\blabel\\b", nm))
  testthat::expect_true(grepl("\\bformat\\b", nm))
  testthat::expect_true(grepl("\\bmy_custom\\b", nm))

  a <- out$attributes[[which(out$column == "A")[1]]]
  testthat::expect_false("label" %in% names(a))
  testthat::expect_true("format" %in% names(a))
  testthat::expect_true("my_custom" %in% names(a))
})

testthat::test_that("get_column_info: label_attr/format_attr priority works", {
  pick1 <- function(x) unname(x)[1]

  df <- data.frame(A = 1:3, stringsAsFactors = FALSE)
  attr(df$A, "Label")  <- "Upper Label"
  attr(df$A, "label")  <- "Lower label wins if first"
  attr(df$A, "Format") <- "FMT_UPPER"
  attr(df$A, "format") <- "FMT_lower"

  out1 <- get_column_info(
    df,
    label_attr  = c("label", "Label"),
    format_attr = c("format", "Format")
  )
  testthat::expect_equal(pick1(out1$label[out1$column == "A"]), "Lower label wins if first")
  testthat::expect_equal(pick1(out1$format[out1$column == "A"]), "FMT_lower")

  out2 <- get_column_info(
    df,
    label_attr  = c("Label", "label"),
    format_attr = c("Format", "format")
  )
  testthat::expect_equal(pick1(out2$label[out2$column == "A"]), "Upper Label")
  testthat::expect_equal(pick1(out2$format[out2$column == "A"]), "FMT_UPPER")
})
