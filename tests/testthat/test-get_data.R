# tests/testthat/test-get_data.R

test_that("get_data: errors on invalid dir input", {
  expect_error(get_data(NULL), "dir must be a single")
  expect_error(get_data(""), "dir must be a single")
  expect_error(get_data("   "), "dir must be a single")
  expect_error(get_data("Z:/this/path/should/not/exist"), "Directory not found")
})

test_that("get_data: errors when no supported files are present", {
  td <- withr::local_tempdir()
  expect_error(get_data(td), "No supported files found in directory")
})

test_that("get_data: loads a single CSV by base name and returns a data.frame", {
  td <- withr::local_tempdir()

  df <- data.frame(USUBJID = c("01", "02"), TRTAN = c(11, 12), stringsAsFactors = FALSE)
  utils::write.csv(df, file.path(td, "adsl.csv"), row.names = FALSE)

  out <- get_data(td, "adsl")
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 2)
  expect_equal(names(out), c("USUBJID", "TRTAN"))
})

test_that("get_data: loads a single CSV by exact file name (case-insensitive)", {
  td <- withr::local_tempdir()

  df <- data.frame(A = 1:2)
  utils::write.csv(df, file.path(td, "ADAE.csv"), row.names = FALSE)

  out <- get_data(td, "adae.csv") # different case
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 2)
})

test_that("get_data: loads multiple CSVs by base names and returns a named list", {
  td <- withr::local_tempdir()

  utils::write.csv(data.frame(A = 1), file.path(td, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(B = 2), file.path(td, "adae.csv"), row.names = FALSE)

  out <- get_data(td, c("adsl", "adae"))
  expect_true(is.list(out))
  expect_named(out, c("adsl", "adae"))
  expect_true(is.data.frame(out$adsl))
  expect_true(is.data.frame(out$adae))
})

test_that("get_data: loads all supported files when file_names=NULL and returns named list", {
  td <- withr::local_tempdir()

  utils::write.csv(data.frame(A = 1), file.path(td, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(B = 1), file.path(td, "adae.csv"), row.names = FALSE)

  out <- get_data(td)
  expect_true(is.list(out))
  expect_setequal(names(out), c("adsl", "adae"))
})

test_that("get_data: detects duplicates by base name (case-insensitive) when file_names=NULL", {
  td <- withr::local_tempdir()

  # same base name, different extension
  utils::write.csv(data.frame(A = 1), file.path(td, "adsl.csv"), row.names = FALSE)
  file.create(file.path(td, "ADSL.xlsx"))

  expect_error(
    get_data(td),
    "Multiple files found with the same base name"
  )
})

test_that("get_data: errors on missing file when file_names provided", {
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(A = 1), file.path(td, "adsl.csv"), row.names = FALSE)

  expect_error(
    get_data(td, "adae"),
    "No matching file found for 'adae'"
  )

  expect_error(
    get_data(td, "adae.csv"),
    "No matching file found for 'adae.csv'"
  )
})

test_that("get_data: errors on multiple matches when user provides base name but duplicates exist", {
  td <- withr::local_tempdir()

  # same base name different extension
  utils::write.csv(data.frame(A = 1), file.path(td, "adlb.csv"), row.names = FALSE)
  file.create(file.path(td, "ADLB.xlsx"))

  # with file_names, base name lookup should error with multiple matches
  expect_error(
    get_data(td, "adlb"),
    "Multiple matching files for 'adlb'"
  )
})

test_that("get_data: returns data.frame for one file and list for multiple files", {
  td <- withr::local_tempdir()

  utils::write.csv(data.frame(A = 1), file.path(td, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(B = 2), file.path(td, "adae.csv"), row.names = FALSE)

  out1 <- get_data(td, "adsl")
  expect_true(is.data.frame(out1))

  out2 <- get_data(td, c("adsl", "adae"))
  expect_true(is.list(out2))
  expect_named(out2, c("adsl", "adae"))
})
