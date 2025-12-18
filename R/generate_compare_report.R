#' Compare DEV vs VAL datasets (PROC COMPARE-style) with robust file detection
#'
#' @description
#' `generate_compare_report()` compares a **developer (DEV)** dataset and a **validation (VAL)**
#' dataset for a given `domain` and produces outputs similar to SAS `PROC COMPARE`.
#'
#' This function is intended for ADaM/SDTM/TFL validation workflows and supports:
#' \itemize{
#'   \item **Directory-driven inputs**: DEV and VAL locations are provided via `dev_dir` and `val_dir`.
#'   \item **Case-insensitive domain matching**: `domain = "ADAE"` will match files like `adae.*`.
#'   \item **VAL prefix flexibility**: resolves `prefix_val` variants such as `v_`, `v-`, and `v` (no separator).
#'   \item **Automatic extension detection** for DEV and VAL files: `.sas7bdat`, `.xpt`, `.csv`, `.rds`.
#'   \item **Optional filtering** using `filter_expr` prior to comparison.
#'   \item **Optional PROC COMPARE-style CSV** output with `BASE`, `COMPARE`, and `DIF` triplets.
#'   \item **Optional LST-like report** using `arsenal::comparedf()` for summarized differences.
#' }
#'
#' @details
#' ## File resolution rules
#' The function looks for exactly one matching domain file per directory:
#' \itemize{
#'   \item DEV: `<domain>.<ext>`
#'   \item VAL: `<prefix><domain>.<ext>` where `<prefix>` is `prefix_val` plus common variants
#'         supporting underscore/hyphen/no-separator forms (e.g., `v_`, `v-`, `v`).
#' }
#'
#' Supported extensions (priority order) are:
#' \code{sas7bdat}, \code{xpt}, \code{csv}, \code{rds}.
#'
#' If multiple matches exist for the same domain in a directory (e.g., `adae.csv` and `adae.xpt`),
#' the function stops with an **ambiguous match** error to prevent accidental comparisons.
#'
#' ## PROC COMPARE-style CSV behavior
#' When `write_csv = TRUE`, the output includes:
#' \itemize{
#'   \item `_TYPE_` with values `BASE`, `COMPARE`, `DIF`
#'   \item `_OBS_` sequence within each BY key
#'   \item For numeric variables, `DIF = DEV - VAL`
#'   \item For Date variables, `DIF` is **integer day difference** (`as.integer(DEV - VAL)`)
#'   \item For POSIXct variables, `DIF` is **seconds difference** (`as.numeric(DEV - VAL)`)
#'   \item For other types, `DIF` is a character mask (`X` indicates difference)
#' }
#'
#' @param domain Character scalar domain name (e.g., `"adsl"`, `"adae"`, `"rt-ae-sum"`).
#'   Matching is case-insensitive.
#' @param dev_dir DEV dataset directory path.
#' @param val_dir VAL dataset directory path.
#' @param by_vars Character vector of key variables used to match records
#'   (e.g., `c("STUDYID","USUBJID")` or `c("STUDYID","USUBJID","AESEQ")`).
#' @param vars_to_check Optional character vector of variables to compare.
#'   If `NULL`, compares all common variables (excluding key handling remains as per implementation).
#' @param report_dir Output directory for report files. Created if missing.
#' @param prefix_val Character prefix for validation datasets (default `"v_"`).
#'   The resolver also supports variants like `v-` and `v` (no separator).
#' @param max_print Maximum number of lines printed in the `.lst` report for summaries/diffs.
#' @param write_csv Logical; if `TRUE`, writes PROC COMPARE-style CSV to `report_dir` as
#'   `compare_<domain>.csv`.
#' @param run_comparedf Logical; if `TRUE`, uses `arsenal::comparedf()` to generate a `.lst` report.
#' @param filter_expr Optional filter expression **string** evaluated within each dataset
#'   (e.g., `"SAFFL == 'Y' & TRTEMFL == 'Y'"`).
#' @param study_id Optional study identifier included in the `.lst` header.
#' @param author Optional author name included in the `.lst` header.
#'
#' @return Invisibly returns a list with:
#' \itemize{
#'   \item `only_in_dev`: rows present only in DEV (set-difference result)
#'   \item `only_in_val`: rows present only in VAL (set-difference result)
#'   \item `comparedf`: `arsenal::comparedf` object (or `NULL` if `run_comparedf = FALSE`)
#' }
#' @importFrom utils head capture.output
#' @importFrom data.table as.data.table fread fwrite rbindlist setkeyv setorderv setcolorder fsetdiff fintersect copy
#' @seealso \code{\link[arsenal]{comparedf}}, \code{\link[data.table]{fsetdiff}},
#'   \code{\link[data.table]{fintersect}}
#'
#' @examples
#' \dontrun{
#' # ------------------------------------------------------------
#' # Example 1: Basic comparison (auto-detect CSV files)
#' # DEV: adae.csv
#' # VAL: v-adae.csv
#' generate_compare_report(
#'   domain    = "adae",
#'   dev_dir   = "C:/R-Packages/adam/dev",
#'   val_dir   = "C:/R-Packages/adam/val",
#'   by_vars   = c("STUDYID","USUBJID","AESEQ"),
#'   write_csv = TRUE
#' )
#'
#' # ------------------------------------------------------------
#' # Example 2: Case-insensitive domain name ("ADAE" matches adae.*)
#' generate_compare_report(
#'   domain  = "ADAE",
#'   dev_dir = "C:/R-Packages/adam/dev",
#'   val_dir = "C:/R-Packages/adam/val",
#'   by_vars = c("STUDYID","USUBJID","AESEQ")
#' )
#'
#' # ------------------------------------------------------------
#' # Example 3: Mixed formats (DEV .sas7bdat, VAL .xpt)
#' # DEV: adsl.sas7bdat
#' # VAL: v_adsl.xpt (or v-adsl.xpt)
#' generate_compare_report(
#'   domain  = "adsl",
#'   dev_dir = "D:/study/dev/adam",
#'   val_dir = "D:/study/val/sasout",
#'   by_vars = c("STUDYID","USUBJID")
#' )
#'
#' # ------------------------------------------------------------
#' # Example 4: Hyphenated domain (e.g., TFL artifacts)
#' # DEV: rt-ae-sum.csv
#' # VAL: v-rt-ae-sum.csv
#' generate_compare_report(
#'   domain    = "rt-ae-sum",
#'   dev_dir   = "C:/R-Packages/adam/dev",
#'   val_dir   = "C:/R-Packages/adam/val",
#'   by_vars   = c("STUDYID","USUBJID","AESEQ"),
#'   write_csv = TRUE
#' )
#'
#' # ------------------------------------------------------------
#' # Example 5: Filtered comparison (subset both datasets)
#' generate_compare_report(
#'   domain      = "adae",
#'   dev_dir     = "C:/adam/dev",
#'   val_dir     = "C:/adam/val",
#'   by_vars     = c("STUDYID","USUBJID","AESEQ"),
#'   filter_expr = "SAFFL == 'Y' & TRTEMFL == 'Y'",
#'   write_csv   = TRUE
#' )
#'
#' # ------------------------------------------------------------
#' # Example 6: Restrict comparison to specific variables only
#' generate_compare_report(
#'   domain        = "adae",
#'   dev_dir       = "C:/adam/dev",
#'   val_dir       = "C:/adam/val",
#'   by_vars       = c("STUDYID","USUBJID","AESEQ"),
#'   vars_to_check = c("AETOXGR", "AEREL", "ASTDT", "AENDT"),
#'   write_csv     = TRUE
#' )
#'
#' # ------------------------------------------------------------
#' # Example 7: CSV only (skip comparedf / LST report)
#' generate_compare_report(
#'   domain        = "adlb",
#'   dev_dir       = "C:/adam/dev",
#'   val_dir       = "C:/adam/val",
#'   by_vars       = c("STUDYID","USUBJID","PARAMCD","AVISITN"),
#'   write_csv     = TRUE,
#'   run_comparedf = FALSE
#' )
#'
#' # ------------------------------------------------------------
#' # Example 8: Compare only key alignment (minimal variable selection)
#' # Useful when you only want to detect missing/extra keys quickly.
#' generate_compare_report(
#'   domain        = "adsl",
#'   dev_dir       = "C:/adam/dev",
#'   val_dir       = "C:/adam/val",
#'   by_vars       = c("STUDYID","USUBJID"),
#'   vars_to_check = c("USUBJID"),   # effectively only key coverage + minimal compare
#'   run_comparedf = FALSE,
#'   write_csv     = TRUE
#' )
#'
#' # ------------------------------------------------------------
#' # Example 9: Ambiguous file error (intentional safety stop)
#' # DEV folder contains both adae.csv and adae.xpt -> stop
#' generate_compare_report(
#'   domain  = "adae",
#'   dev_dir = "C:/adam/dev",
#'   val_dir = "C:/adam/val",
#'   by_vars = c("STUDYID","USUBJID","AESEQ")
#' )
#' }
#' @export
generate_compare_report <- function(domain,
                                    dev_dir,
                                    val_dir,
                                    by_vars       = c("STUDYID", "USUBJID"),
                                    vars_to_check = NULL,
                                    report_dir    = getwd(),
                                    prefix_val    = "v_",
                                    max_print     = 50,
                                    write_csv     = FALSE,
                                    run_comparedf = TRUE,
                                    filter_expr   = NULL,
                                    study_id      = NULL,
                                    author        = NULL) {

  # ------------------------------------------------------------------
  # Dependency checks (explicit, early, readable errors)
  # ------------------------------------------------------------------
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package 'data.table' is required by generate_compare_report(), but it is not installed.\n",
      "Install it with: install.packages('data.table')"
    )
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "Package 'haven' is required by generate_compare_report() to read .sas7bdat/.xpt.\n",
      "Install it with: install.packages('haven')"
    )
  }
  if (isTRUE(run_comparedf) && !requireNamespace("arsenal", quietly = TRUE)) {
    stop(
      "run_comparedf=TRUE requires package 'arsenal'.\n",
      "Install it with: install.packages('arsenal')\n",
      "Or set run_comparedf = FALSE."
    )
  }

  # Make data.table symbols available without attaching
  `%chin%` <- data.table::`%chin%`

  # ------------------------------------------------------------------
  # Validate inputs
  # ------------------------------------------------------------------
  if (!is.character(domain) || length(domain) != 1 || !nzchar(trimws(domain))) {
    stop("domain must be a single, non-empty string (e.g., 'adae').")
  }
  if (!is.character(dev_dir) || length(dev_dir) != 1 || !nzchar(trimws(dev_dir))) {
    stop("dev_dir must be a single, non-empty path string.")
  }
  if (!is.character(val_dir) || length(val_dir) != 1 || !nzchar(trimws(val_dir))) {
    stop("val_dir must be a single, non-empty path string.")
  }
  if (!dir.exists(dev_dir)) stop("DEV directory not found: ", dev_dir)
  if (!dir.exists(val_dir)) stop("VAL directory not found: ", val_dir)

  if (!is.character(by_vars) || length(by_vars) < 1) {
    stop("by_vars must be a non-empty character vector.")
  }

  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ------------------------------------------------------------------
  # Helpers: resolve file + read based on extension
  # ------------------------------------------------------------------
  .supported_exts <- function() c("sas7bdat", "xpt", "csv", "rds")

  .normalize_prefixes <- function(prefix_val) {
    p0 <- as.character(prefix_val)[1]
    if (is.na(p0) || !nzchar(p0)) return(character())

    # variants for user convenience: keep original, swap _/- and remove trailing sep
    p1 <- gsub("_", "-", p0, fixed = TRUE)
    p2 <- gsub("-", "_", p0, fixed = TRUE)
    p3 <- sub("[-_]$", "", p0)   # drop trailing '_' or '-'
    unique(c(p0, p1, p2, p3))
  }

  .resolve_domain_file <- function(dir_path, domain, prefixes = NULL, label = "DEV") {
    dom  <- tolower(trimws(domain))
    exts <- .supported_exts()

    # Candidate basenames (priority order): .sas7bdat > .xpt > .csv > .rds
    if (is.null(prefixes) || length(prefixes) == 0) {
      candidates <- paste0(dom, ".", exts)
    } else {
      prefixes <- prefixes[!is.na(prefixes) & nzchar(prefixes)]
      candidates <- as.vector(outer(prefixes, paste0(dom, ".", exts), paste0))
    }

    files <- list.files(dir_path, full.names = TRUE, recursive = FALSE)
    if (length(files) > 0) {
      bn <- tolower(basename(files))
      hits <- files[bn %chin% tolower(candidates)]
      if (length(hits) == 1) return(hits)
      if (length(hits) > 1) {
        stop(
          "Ambiguous ", label, " file for domain='", domain, "' in: ", dir_path, "\n",
          "Multiple matches found:\n- ", paste(hits, collapse = "\n- "), "\n",
          "Please keep only one file per domain in that directory."
        )
      }
    }

    # Helpful diagnostic list (portable: no perl=TRUE)
    pat <- paste0("\\.(", paste(exts, collapse = "|"), ")$")
    all_supported <- list.files(dir_path, pattern = pat, ignore.case = TRUE)

    expected <- if (is.null(prefixes) || length(prefixes) == 0) {
      paste0(dom, ".{", paste(exts, collapse = "|"), "}")
    } else {
      paste0("[", paste(unique(prefixes), collapse = " | "), "]", dom, ".{", paste(exts, collapse = "|"), "}")
    }

    stop(
      label, " dataset not found for domain='", domain, "' in: ", dir_path, "\n",
      "Expected one of: ", expected, "\n",
      "Available files (supported extensions): ",
      if (length(all_supported) == 0) "<none>" else paste(all_supported, collapse = ", ")
    )
  }

  .read_any_to_dt <- function(path) {
    ext <- tolower(sub("^.*\\.", "", basename(path)))

    if (ext == "sas7bdat") {
      df <- haven::read_sas(path)
      return(data.table::as.data.table(df))
    }
    if (ext == "xpt") {
      df <- haven::read_xpt(path)
      return(data.table::as.data.table(df))
    }
    if (ext == "csv") {
      return(data.table::fread(path, na.strings = c("", "NA", "NaN", ".")))
    }
    if (ext == "rds") {
      obj <- readRDS(path)
      return(data.table::as.data.table(obj))
    }

    stop("Unsupported extension: .", ext, " for file: ", path)
  }

  # ------------------------------------------------------------------
  # Resolve DEV/VAL file paths (case-insensitive, prefix-flexible)
  # ------------------------------------------------------------------
  dev_path <- .resolve_domain_file(dev_dir, domain, prefixes = NULL, label = "DEV")

  val_prefixes <- .normalize_prefixes(prefix_val)
  val_path <- .resolve_domain_file(val_dir, domain, prefixes = val_prefixes, label = "VAL")

  message("[INFO] DEV file: ", dev_path)
  message("[INFO] VAL file: ", val_path)
  message("[INFO] Reading datasets...")

  dev_info <- file.info(dev_path)
  val_info <- file.info(val_path)

  dev_df <- .read_any_to_dt(dev_path)
  val_df <- .read_any_to_dt(val_path)

  raw_dev_n    <- nrow(dev_df)
  raw_val_n    <- nrow(val_df)
  raw_dev_vars <- ncol(dev_df)
  raw_val_vars <- ncol(val_df)

  # ------------------------------------------------------------------
  # Filtering (kept as-is: string evaluated via parse)
  # ------------------------------------------------------------------
  if (!is.null(filter_expr)) {
    dev_df <- dev_df[eval(parse(text = filter_expr))]
    val_df <- val_df[eval(parse(text = filter_expr))]
  }

  # ------------------------------------------------------------------
  # Harmonize columns (kept as-is)
  # ------------------------------------------------------------------
  missing_in_dev <- setdiff(names(val_df), names(dev_df))
  missing_in_val <- setdiff(names(dev_df), names(val_df))

  common_cols <- intersect(names(dev_df), names(val_df))
  dev_df <- dev_df[, ..common_cols]
  val_df <- val_df[, ..common_cols]

  # Type harmonization (kept as-is)
  for (col in common_cols) {
    class_dev <- class(dev_df[[col]])
    class_val <- class(val_df[[col]])
    if (!identical(class_dev, class_val)) {
      suppressWarnings({
        if ("Date" %in% class_dev) {
          val_df[[col]] <- as.Date(val_df[[col]])
        } else if ("POSIXct" %in% class_dev) {
          val_df[[col]] <- as.POSIXct(val_df[[col]])
        } else if ("numeric" %in% class_dev) {
          val_df[[col]] <- as.numeric(val_df[[col]])
        } else if ("character" %in% class_dev) {
          val_df[[col]] <- as.character(val_df[[col]])
        } else if ("integer" %in% class_dev) {
          val_df[[col]] <- as.integer(val_df[[col]])
        }
      })
    }
  }

  # Keying
  data.table::setkeyv(dev_df, by_vars)
  data.table::setkeyv(val_df, by_vars)

  only_in_dev <- data.table::fsetdiff(dev_df, val_df)
  only_in_val <- data.table::fsetdiff(val_df, dev_df)

  # ------------------------------------------------------------------
  # PROC COMPARE-style CSV (updated to avoid Date/POSIXct coercion warnings)
  # ------------------------------------------------------------------
  if (isTRUE(write_csv)) {

    .char_diff_mask <- function(a, b) {
      a <- ifelse(is.na(a), "", as.character(a))
      b <- ifelse(is.na(b), "", as.character(b))
      na <- nchar(a, type = "chars", allowNA = FALSE)
      nb <- nchar(b, type = "chars", allowNA = FALSE)
      n  <- pmax(na, nb)
      if (all(n == 0L)) return(rep("", length(a)))
      ap <- ifelse(na < n, paste0(a, strrep(" ", n - na)), a)
      bp <- ifelse(nb < n, paste0(b, strrep(" ", n - nb)), b)
      vapply(seq_along(ap), function(i) {
        if (n[i] == 0L) return("")
        ai <- unlist(strsplit(substr(ap[i], 1, n[i]), "", fixed = TRUE))
        bi <- unlist(strsplit(substr(bp[i], 1, n[i]), "", fixed = TRUE))
        if (all(ai == bi)) paste(rep(".", n[i]), collapse = "")
        else paste(ifelse(ai == bi, " ", "X"), collapse = "")
      }, FUN.VALUE = character(1))
    }

    vars_pool <- names(dev_df)
    cmp_vars <- if (!is.null(vars_to_check)) {
      intersect(vars_to_check, vars_pool)
    } else {
      setdiff(vars_pool, by_vars)
    }
    if (length(cmp_vars) == 0L) cmp_vars <- setdiff(vars_pool, by_vars)

    matched_keys <- data.table::fintersect(dev_df[, ..by_vars], val_df[, ..by_vars])

    # one record per key (kept as-is)
    dev_match <- dev_df[matched_keys, on = by_vars][, .SD[1], by = by_vars]
    val_match <- val_df[matched_keys, on = by_vars][, .SD[1], by = by_vars]

    base_wide    <- data.table::copy(dev_match)[, c(by_vars, cmp_vars), with = FALSE]
    compare_wide <- data.table::copy(val_match)[, c(by_vars, cmp_vars), with = FALSE]
    base_wide[,    `_TYPE_` := "BASE"]
    compare_wide[, `_TYPE_` := "COMPARE"]

    dif_wide <- dev_match[, c(by_vars), with = FALSE]
    dif_wide[, `_TYPE_` := "DIF"]

    # DIF logic:
    # - numeric: dv - vv
    # - Date:    as.integer(Date(dev) - Date(val))  (days)
    # - POSIXct: as.numeric(dev - val)              (seconds)
    # - else:    character mask
    for (v in cmp_vars) {
      dv <- dev_match[[v]]
      vv <- val_match[[v]]

      if (is.numeric(dv) && is.numeric(vv)) {
        dif_wide[[v]] <- dv - vv
        next
      }

      if (inherits(dv, "Date") || inherits(vv, "Date")) {
        dif_wide[[v]] <- as.integer(as.Date(dv) - as.Date(vv))
        next
      }

      if (inherits(dv, "POSIXct") || inherits(vv, "POSIXct")) {
        dif_wide[[v]] <- as.numeric(as.POSIXct(dv) - as.POSIXct(vv))
        next
      }

      dif_wide[[v]] <- .char_diff_mask(as.character(dv), as.character(vv))
    }

    out_wide <- data.table::rbindlist(list(base_wide, compare_wide, dif_wide),
                                      use.names = TRUE, fill = TRUE)

    only_dev_keys <- data.table::fsetdiff(unique(only_in_dev[, ..by_vars]), matched_keys)
    only_val_keys <- data.table::fsetdiff(unique(only_in_val[, ..by_vars]), matched_keys)

    if (nrow(only_dev_keys)) {
      dev_only1 <- only_in_dev[only_dev_keys, on = by_vars][, .SD[1], by = by_vars]

      base2 <- data.table::copy(dev_only1)[, c(by_vars, cmp_vars), with = FALSE][, `_TYPE_` := "BASE"]
      comp2 <- dev_only1[, c(by_vars), with = FALSE][, `_TYPE_` := "COMPARE"]
      for (v in cmp_vars) comp2[[v]] <- NA

      dif2  <- dev_only1[, c(by_vars), with = FALSE][, `_TYPE_` := "DIF"]
      for (v in cmp_vars) {
        dv <- dev_only1[[v]]
        if (is.numeric(dv)) {
          dif2[[v]] <- NA_real_
        } else if (inherits(dv, "Date")) {
          dif2[[v]] <- NA_integer_
        } else if (inherits(dv, "POSIXct")) {
          dif2[[v]] <- NA_real_
        } else {
          dif2[[v]] <- .char_diff_mask(as.character(dv), NA_character_)
        }
      }

      out_wide <- data.table::rbindlist(list(out_wide, base2, comp2, dif2),
                                        use.names = TRUE, fill = TRUE)
    }

    if (nrow(only_val_keys)) {
      val_only1 <- only_in_val[only_val_keys, on = by_vars][, .SD[1], by = by_vars]

      comp3 <- data.table::copy(val_only1)[, c(by_vars, cmp_vars), with = FALSE][, `_TYPE_` := "COMPARE"]
      base3 <- val_only1[, c(by_vars), with = FALSE][, `_TYPE_` := "BASE"]
      for (v in cmp_vars) base3[[v]] <- NA

      dif3  <- val_only1[, c(by_vars), with = FALSE][, `_TYPE_` := "DIF"]
      for (v in cmp_vars) {
        vv <- val_only1[[v]]
        if (is.numeric(vv)) {
          dif3[[v]] <- NA_real_
        } else if (inherits(vv, "Date")) {
          dif3[[v]] <- NA_integer_
        } else if (inherits(vv, "POSIXct")) {
          dif3[[v]] <- NA_real_
        } else {
          dif3[[v]] <- .char_diff_mask(NA_character_, as.character(vv))
        }
      }

      out_wide <- data.table::rbindlist(list(out_wide, base3, comp3, dif3),
                                        use.names = TRUE, fill = TRUE)
    }

    out_wide[, `_TYPE_` := factor(`_TYPE_`, levels = c("BASE", "COMPARE", "DIF"))]
    data.table::setorderv(out_wide, c(by_vars, "_TYPE_"))
    out_wide[, `_OBS_` := seq_len(.N), by = by_vars]

    col_order <- c("_TYPE_", "_OBS_", by_vars, cmp_vars)
    col_order <- col_order[col_order %chin% names(out_wide)]
    data.table::setcolorder(out_wide, col_order)

    out_csv <- file.path(report_dir, paste0("compare_", tolower(domain), ".csv"))
    data.table::fwrite(out_wide, out_csv)
    message("[OK] PROC COMPARE-style CSV written to: ", out_csv)
  }

  # ------------------------------------------------------------------
  # comparedf report (kept as-is)
  # ------------------------------------------------------------------
  matched_keys <- data.table::fintersect(dev_df[, ..by_vars], val_df[, ..by_vars])
  dev_match <- dev_df[matched_keys, on = by_vars]
  val_match <- val_df[matched_keys, on = by_vars]

  if (!is.null(vars_to_check)) {
    vars_to_use <- intersect(vars_to_check, names(dev_match))
    dev_match <- dev_match[, ..vars_to_use]
    val_match <- val_match[, ..vars_to_use]
  }

  if (isTRUE(run_comparedf)) {
    comp <- arsenal::comparedf(
      x = dev_match,
      y = val_match,
      by = by_vars,
      control = arsenal::comparedf.control(
        max.print.diffs = max_print,
        tolerance = 1e-6,
        ignore.row.order = TRUE
      )
    )

    info      <- Sys.info()
    user_name <- info["user"]
    user_sys  <- info["sysname"]
    user_vers <- info["release"]
    ts        <- Sys.time()
    domain_   <- toupper(domain)

    report_file <- file.path(report_dir, paste0(prefix_val, tolower(domain), ".lst"))
    sink(report_file)
    options(width = 180)

    cat(strrep("=", 150), "\n")
    cat("R Comparison Report (Like PROC COMPARE) for:", domain_, "\n")
    cat(strrep("=", 150), "\n\n")
    cat("Generated on:  ", format(ts), "\n")
    if (!is.null(study_id)) cat("Study ID:       ", study_id, "\n")
    if (!is.null(author))   cat("Author:         ", author, "\n")
    cat("User:          ", user_name, "\n")
    cat("System:        ", user_sys, "\n")
    cat("OS Version:    ", user_vers, "\n\n")
    cat("Developer file:  ", dev_path, "\n")
    cat("Validation file: ", val_path, "\n\n")

    cat("Dataset Overview:\n\n")
    cat("  - Developer Dataset    :                   ", format(raw_dev_n, big.mark = ","), " rows, ", raw_dev_vars, " variables\n")
    cat("     Date & Time Created :                   ", format(dev_info$mtime), "\n\n")

    cat("  - Validation Dataset   :                   ", format(raw_val_n, big.mark = ","), " rows, ", raw_val_vars, " variables\n")
    cat("     Date & Time Created :                   ", format(val_info$mtime), "\n\n")

    cat("  - Variables Used in Comparison:            ", length(common_cols), "\n")
    cat("  - Rows Compared:                           ", format(nrow(dev_df), big.mark = ","), "\n\n")

    cat("Filtered Dataset Sizes After Preprocessing:\n")
    cat("   Filtered DEV:   ", format(nrow(dev_df), big.mark = ","), "\n")
    cat("   Filtered VAL:   ", format(nrow(val_df), big.mark = ","), "\n")
    cat("   Only in DEV:    ", format(nrow(only_in_dev), big.mark = ","), "\n")
    cat("   Only in VAL:    ", format(nrow(only_in_val), big.mark = ","), "\n\n")

    if (length(missing_in_dev) > 0 || length(missing_in_val) > 0) {
      cat(strrep("-", 150), "\n")
      cat("Missing Variables:\n")
      if (length(missing_in_dev) > 0) {
        cat("  In Validation but not in Dev:\n")
        for (i in seq_along(missing_in_dev)) cat(sprintf("    %2d. %s\n", i, missing_in_dev[i]))
      }
      if (length(missing_in_val) > 0) {
        cat("  In Dev but not in Validation:\n")
        for (i in seq_along(missing_in_val)) cat(sprintf("    %2d. %s\n", i, missing_in_val[i]))
      }
      cat(strrep("-", 150), "\n\n")
    }

    cat(strrep("=", 150), "\n\n")
    cat(" Summary of Differences (limited to first ", max_print, " lines):\n\n")
    cat(paste(head(capture.output(summary(comp)), max_print), collapse = "\n"), "\n\n")
    cat(" Detailed Diffs (first ", max_print, " lines):\n\n")
    cat(paste(head(capture.output(print(comp$diffs)), max_print), collapse = "\n"), "\n")

    sink()
    message("Comparison report with diffs written to: ", report_file)
  } else {
    comp <- NULL
  }

  invisible(list(
    only_in_dev = only_in_dev,
    only_in_val = only_in_val,
    comparedf   = comp
  ))
}
