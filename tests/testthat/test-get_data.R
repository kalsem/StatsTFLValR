# tests/testthat/test-get_data.R

testthat::test_that("get_data: loads a single CSV and returns it invisibly; assigns in .GlobalEnv", {
  tmp <- withr::local_tempdir()

  # Create a small CSV
  df <- data.frame(A = 1:3, B = c("a","b","c"), stringsAsFactors = FALSE)
  f  <- file.path(tmp, "adsl.csv")
  utils::write.csv(df, f, row.names = FALSE)

  # Ensure clean global before test
  if (exists("adsl", envir = .GlobalEnv, inherits = FALSE)) {
    rm(adsl, envir = .GlobalEnv)
  }

  out <- get_data(tmp, "adsl")  # base name

  # Returned value
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true(all(c("A","B") %in% names(out)))

  # Assigned to GlobalEnv with base name
  testthat::expect_true(exists("adsl", envir = .GlobalEnv, inherits = FALSE))
  g <- get("adsl", envir = .GlobalEnv, inherits = FALSE)
  testthat::expect_s3_class(g, "data.frame")
  testthat::expect_equal(nrow(g), 3)
})

testthat::test_that("get_data: loads by exact file name with extension (case-insensitive match)", {
  tmp <- withr::local_tempdir()

  df <- data.frame(A = 1:2, stringsAsFactors = FALSE)
  utils::write.csv(df, file.path(tmp, "ADAE.csv"), row.names = FALSE)

  if (exists("ADAE", envir = .GlobalEnv, inherits = FALSE)) {
    rm(ADAE, envir = .GlobalEnv)
  }

  out <- get_data(tmp, "adae.csv")  # exact filename but different case

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(exists("ADAE", envir = .GlobalEnv, inherits = FALSE))
})

testthat::test_that("get_data: file not found errors (base name and exact filename)", {
  tmp <- withr::local_tempdir()

  testthat::expect_error(
    get_data(tmp, "adae"),
    regexp = "No matching file found for 'adae' in directory",
    fixed  = FALSE
  )

  testthat::expect_error(
    get_data(tmp, "adae.csv"),
    regexp = "No matching file found for 'adae\\.csv' in directory",
    fixed  = FALSE
  )
})

testthat::test_that("get_data: loads multiple datasets by base names and returns NULL invisibly", {
  tmp <- withr::local_tempdir()

  utils::write.csv(data.frame(A=1), file.path(tmp, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(B=2), file.path(tmp, "adae.csv"), row.names = FALSE)

  # Clean global vars if present
  if (exists("adsl", envir = .GlobalEnv, inherits = FALSE)) rm(adsl, envir = .GlobalEnv)
  if (exists("adae", envir = .GlobalEnv, inherits = FALSE)) rm(adae, envir = .GlobalEnv)

  out <- get_data(tmp, c("adsl", "adae"))

  # Multiple => invisible NULL
  testthat::expect_true(is.null(out))

  # Both assigned
  testthat::expect_true(exists("adsl", envir = .GlobalEnv, inherits = FALSE))
  testthat::expect_true(exists("adae", envir = .GlobalEnv, inherits = FALSE))
})

testthat::test_that("get_data: loads all supported files when file_names=NULL (no duplicates)", {
  tmp <- withr::local_tempdir()

  utils::write.csv(data.frame(A=1), file.path(tmp, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(B=1), file.path(tmp, "adae.csv"), row.names = FALSE)

  if (exists("adsl", envir = .GlobalEnv, inherits = FALSE)) rm(adsl, envir = .GlobalEnv)
  if (exists("adae", envir = .GlobalEnv, inherits = FALSE)) rm(adae, envir = .GlobalEnv)

  out <- get_data(tmp)

  # Multiple => invisible NULL
  testthat::expect_true(is.null(out))

  # Assigned
  testthat::expect_true(exists("adsl", envir = .GlobalEnv, inherits = FALSE))
  testthat::expect_true(exists("adae", envir = .GlobalEnv, inherits = FALSE))
})

testthat::test_that("get_data: duplicates error when file_names=NULL and same base name exists with different extensions", {
  tmp <- withr::local_tempdir()

  # Same base name, different extensions (we can just create empty files; reading won't occur because error happens first)
  file.create(file.path(tmp, "adsl.csv"))
  file.create(file.path(tmp, "ADSL.xlsx"))

  testthat::expect_error(
    get_data(tmp, file_names = NULL),
    regexp = "Multiple files found with the same base name|Please resolve file name conflicts",
    fixed  = FALSE
  )
})

testthat::test_that("get_data: duplicates error for explicit base-name selection if multiple matches exist", {
  tmp <- withr::local_tempdir()

  file.create(file.path(tmp, "adlb.csv"))
  file.create(file.path(tmp, "ADLB.xlsx"))

  testthat::expect_error(
    get_data(tmp, "adlb"),
    regexp = "Multiple matching files for 'adlb'|Please specify the exact file name with extension",
    fixed  = FALSE
  )
})

testthat::test_that("get_data: file_names with extension must match exact filename (case-insensitive)", {
  tmp <- withr::local_tempdir()

  utils::write.csv(data.frame(A=1), file.path(tmp, "adsl.csv"), row.names = FALSE)

  # Ask for non-existing exact file
  testthat::expect_error(
    get_data(tmp, "adsl.xlsx"),
    regexp = "No matching file found for 'adsl\\.xlsx'",
    fixed  = FALSE
  )
})

testthat::test_that("get_data: return value is NULL for multiple loads and data.frame for single load", {
  tmp <- withr::local_tempdir()

  utils::write.csv(data.frame(A=1), file.path(tmp, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(B=1), file.path(tmp, "adae.csv"), row.names = FALSE)

  out1 <- get_data(tmp, "adsl")
  testthat::expect_s3_class(out1, "data.frame")

  out2 <- get_data(tmp, c("adsl", "adae"))
  testthat::expect_true(is.null(out2))
})
