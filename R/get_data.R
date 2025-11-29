#' Load Data Files of Various Formats into Global Environment
#'
#' Loads one or more data files from a given directory into the global environment.
#' Supports multiple file types commonly used in clinical trials: `.sas7bdat`, `.xpt`, `.csv`, `.xls`, and `.xlsx`.
#'
#' Automatically detects file extensions and assigns each dataset using its base file name (e.g., `"adsl.xpt"` becomes `adsl`).
#'
#' If multiple files with the same base name but different extensions exist (e.g., `adsl.csv` and `adsl.sas7bdat`), the function
#' will stop and report the duplicates to avoid ambiguity. This validation applies both when reading all files and when a list of files is specified.
#'
#' @param dir Character. Path to the directory containing data files.
#' @param file_names Character vector. Optional base names (with or without extensions) to load;
#'   if NULL, loads all supported files from the directory.
#'
#' @return Invisibly returns the single data frame if one file is loaded, or NULL if multiple.
#' In all cases, assigns each loaded data frame to the global environment using its base name.
#'
#' @importFrom haven read_sas read_xpt
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @examples
#' \dontrun{
#'
#'   # ------------------------------------------------------------------
#'   # 1) Initialize directory structure (Windows example)
#'   # ------------------------------------------------------------------
#'
#'   # Use proper slashes in Windows paths:
#'   adam_ds <- "C:/R-Packages/adam/"
#'   # OR: adam_ds <- "C:\\R-Packages\\adam\\"
#'
#'
#'   # ------------------------------------------------------------------
#'   # 2) Load a single dataset from ADaM
#'   # ------------------------------------------------------------------
#'
#'   adae <- get_data(adam_ds, "adae")
#'
#'
#'   # ------------------------------------------------------------------
#'   # 3) Load multiple datasets by base name
#'   # ------------------------------------------------------------------
#'
#'   get_data(adam_ds, c("adsl", "adae"))
#'
#'
#'   # ------------------------------------------------------------------
#'   # 4) Load all supported datasets from a directory
#'   # ------------------------------------------------------------------
#'
#'   get_data(adam_ds)
#'
#'
#'   # ------------------------------------------------------------------
#'   # 5) Example: duplicate base names trigger an error
#'   # ------------------------------------------------------------------
#'
#'   # If your folder contains files like:
#'   #   adlb.sas7bdat
#'   #   ADLB.xpt
#'   #   admh.sas7bdat
#'   #   ADMH.xpt
#'   #
#'   # Then the following will throw:
#'   #
#'   #   Error in get_data(adam_ds):
#'   #     Multiple files found with the same base name (different extensions):
#'   #       - adlb.sas7bdat
#'   #       - ADLB.xpt
#'   #       - admh.sas7bdat
#'   #       - ADMH.xpt
#'   #     Please resolve file name conflicts or use only one extension per base name.
#'   #
#'   # get_data(adam_ds)
#'
#' }

#' @export
get_data <- function(dir, file_names = NULL) {
  exts <- c("sas7bdat", "xpt", "csv", "xls", "xlsx")
  pattern <- paste0("\\.(", paste(exts, collapse = "|"), ")$")

  all_files <- ps_list_files(dir, pattern = pattern, ignore.case = TRUE)
  all_basenames <- tools::file_path_sans_ext(tolower(all_files))

  if (is.null(file_names)) {
    dupes <- all_basenames[duplicated(all_basenames)]
    if (length(dupes)) {
      dup_files <- all_files[tolower(tools::file_path_sans_ext(all_files)) %in% dupes]
      stop("Multiple files found with the same base name (different extensions):\n",
           paste(" -", dup_files, collapse = "\n"),
           "\nPlease resolve file name conflicts or use only one extension per base name.")
    }
    files <- all_files
  } else {
    files <- vapply(file_names, function(f) {
      has_ext <- grepl(paste0("\\.(", paste(exts, collapse = "|"), ")$"), tolower(f))
      if (has_ext) {
        # Treat as exact file name
        matched_file <- all_files[tolower(all_files) == tolower(f)]
        if (length(matched_file) == 0) {
          stop(sprintf("No matching file found for '%s' in directory: %s", f, dir))
        }
        return(matched_file)
      } else {
        # Treat as base name, and find matching file(s)
        base <- tolower(f)
        matches <- all_files[
          tolower(tools::file_path_sans_ext(all_files)) == base
        ]
        if (length(matches) == 0) {
          stop(sprintf("No matching file found for '%s' in directory: %s", f, dir))
        }
        if (length(matches) > 1) {
          stop(sprintf("Multiple matching files for '%s': %s\nPlease specify the exact file name with extension.",
                       f, paste(matches, collapse = ", ")))
        }
        return(matches)
      }
    }, character(1))
  }

  results <- vector("list", length(files))
  names(results) <- tools::file_path_sans_ext(files)

  for (fn in files) {
    ps_message("Loading file: ", fn)
    ext <- tolower(tools::file_ext(fn))
    path <- file.path(dir, fn)
    df <- switch(ext,
                 # sas7bdat = haven::read_sas(path),
                 # xpt      = haven::read_xpt(path),
                 # csv      = utils::read.csv(path, stringsAsFactors = FALSE),
                 # xls      = readxl::read_excel(path),
                 # xlsx     = readxl::read_excel(path),
                 sas7bdat = ps_read_sas(path),
                 xpt      = ps_read_xpt(path),
                 csv      = ps_read_csv(path, stringsAsFactors = FALSE),
                 xls      = ps_read_excel(path),
                 xlsx     = ps_read_excel(path),
                 stop("Unsupported file extension: ", ext)
    )
    name <- tools::file_path_sans_ext(fn)
    ps_assign(name, df, envir = .GlobalEnv)
    results[[name]] <- df
  }

  if (length(results) == 1) invisible(results[[1]]) else invisible(NULL)
}

