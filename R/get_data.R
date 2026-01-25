#' Load Data Files of Various Formats
#'
#' Loads one or more data files from a given directory.
#' Supports multiple file types commonly used in clinical trials:
#' `.sas7bdat`, `.xpt`, `.csv`, `.xls`, and `.xlsx`.
#'
#' Automatically detects file extensions and returns each dataset using its
#' base file name (e.g., `"adsl.xpt"` becomes `adsl`).
#'
#' If multiple files with the same base name but different extensions exist
#' (e.g., `adsl.csv` and `adsl.sas7bdat`), the function stops and reports the
#' duplicates to avoid ambiguity.
#'
#' @param dir Character. Path to the directory containing data files.
#' @param file_names Character vector. Optional base names (with or without extensions)
#'   to load; if `NULL`, loads all supported files from the directory.
#'
#' @return
#' If exactly one file is loaded, returns the dataset.
#' If multiple files are loaded, returns a named list of datasets.
#'
#' @importFrom haven read_sas read_xpt
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @examples
#' \dontrun{
#'
#' adsl <- get_data("path/to/adam", "adsl")
#'
#' ds <- get_data("path/to/adam")
#'
#' adsl <- ds$adsl
#'
#' }
#'
#' @export
get_data <- function(dir, file_names = NULL) {

  if (!is.character(dir) || length(dir) != 1L || !nzchar(trimws(dir))) {
    stop("dir must be a single, non-empty path string.")
  }
  if (!dir.exists(dir)) {
    stop("Directory not found: ", dir)
  }

  exts <- c("sas7bdat", "xpt", "csv", "xls", "xlsx")
  pattern <- paste0("\\.(", paste(exts, collapse = "|"), ")$")

  all_files <- base::list.files(dir, pattern = pattern, ignore.case = TRUE, full.names = FALSE)

  if (length(all_files) == 0L) {
    stop("No supported files found in directory: ", dir)
  }

  base_lower <- tolower(tools::file_path_sans_ext(all_files))

  if (is.null(file_names)) {

    dupes <- base_lower[duplicated(base_lower)]
    if (length(dupes)) {
      dup_files <- all_files[base_lower %in% dupes]
      stop(
        "Multiple files found with the same base name (different extensions):\n",
        paste(" -", dup_files, collapse = "\n"),
        "\nPlease resolve file name conflicts or keep only one extension per base name."
      )
    }
    files <- all_files

  } else {

    if (!is.character(file_names) || length(file_names) < 1L) {
      stop("file_names must be a character vector or NULL.")
    }

    files <- vapply(file_names, function(f) {
      f <- as.character(f)[1]
      if (!nzchar(trimws(f))) stop("file_names contains an empty value.")

      has_ext <- grepl(paste0("\\.(", paste(exts, collapse = "|"), ")$"), tolower(f))

      if (has_ext) {
        matched <- all_files[tolower(all_files) == tolower(f)]
        if (length(matched) == 0L) {
          stop(sprintf("No matching file found for '%s' in directory: %s", f, dir))
        }
        return(matched[1])
      } else {
        base <- tolower(f)
        matches <- all_files[tolower(tools::file_path_sans_ext(all_files)) == base]
        if (length(matches) == 0L) {
          stop(sprintf("No matching file found for '%s' in directory: %s", f, dir))
        }
        if (length(matches) > 1L) {
          stop(sprintf(
            "Multiple matching files for '%s': %s\nPlease specify the exact file name with extension.",
            f, paste(matches, collapse = ", ")
          ))
        }
        return(matches[1])
      }
    }, character(1))
  }

  results <- vector("list", length(files))
  names(results) <- tools::file_path_sans_ext(files)

  for (fn in files) {
    ext <- tolower(tools::file_ext(fn))
    path <- file.path(dir, fn)

    results[[tools::file_path_sans_ext(fn)]] <- switch(
      ext,
      sas7bdat = haven::read_sas(path),
      xpt      = haven::read_xpt(path),
      csv      = utils::read.csv(path, stringsAsFactors = FALSE),
      xls      = readxl::read_excel(path),
      xlsx     = readxl::read_excel(path),
      stop("Unsupported file extension: ", ext)
    )
  }

  if (length(results) == 1L) {
    return(results[[1]])
  }
  results
}
