#' Summary Table: Mean and Related Statistics by Group
#'
#' This function calculates common summary statistics (N, Mean, SD, Median, Q1, Q3, Min, Max) for a
#' numeric variable, grouped by a treatment or category variable.
#' It supports optional **SAS-style rounding** (round half away from zero) and formats the results
#' for table-ready display.
#' Missing treatment groups are automatically added with zero values.
#'
#' @param data A data frame or tibble containing the input data.
#' @param group_var The grouping variable (e.g., treatment arm).
#'   Can be unquoted (tidy evaluation) or a string.
#' @param uniq_var The numeric variable to summarise.
#'   Can be unquoted (tidy evaluation) or a string.
#' @param label Character string: table section label for the output (e.g., `"BMI (WEIGHT [KG]/ HEIGHT [M2])"`).
#' @param sec_ord Integer: section order value (for downstream table ordering).
#' @param precision_override Optional integer to manually set decimal precision; if `NULL`, the function infers precision from the data.
#' @param indent Integer: number of leading spaces in statistic labels (default = 3).
#' @param use_sas_round Logical: if `TRUE`, applies SAS-compatible rounding (round half away from zero). Default is `FALSE`.
#' @param id_var Character: name of subject ID variable (default = `"USUBJID"`).
#'   If not found, function attempts to auto-detect common ID variable names.
#'
#' @details
#' The function:
#' 1. Auto-detects precision if `precision_override` is `NULL`.
#' 2. Calculates N, mean, SD, quartiles, min, max.
#' 3. Applies SAS-style rounding if `use_sas_round = TRUE`.
#' 4. Converts statistics into a display format suitable for RTF or text output.
#' 5. Ensures all treatment columns appear in output, filling missing ones with `"0"`.
#'
#' **SAS-style rounding logic:**
#' Values exactly halfway between two increments are rounded away from zero
#' (e.g., `1.25` → `1.3`, `-1.25` → `-1.3` with 1 decimal place).
#'
#' @return A tibble with the following columns:
#' - `stats` : internal statistic code (`n1`, `mn`, `sd`, etc.)
#' - `stat`  : display label (`"   N"`, `"   MEAN"`, etc.)
#' - `sort_ord` : row ordering number
#' - `sec_ord` : section ordering number (from input)
#' - Treatment columns (`trt1`, `trt2`, ...): formatted values per treatment group
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1: Basic usage with inferred precision
#' df <- tibble::tibble(
#'   USUBJID = rep(1:6, each = 1),
#'   TRTAN   = c(1, 1, 2, 2, 3, 3),
#'   BMIBL   = c(25.1, 26.3, 24.8, NA, 23.4, 27.6)
#' )
#' mean_by(
#'   data          = df,
#'   group_var     = TRTAN,
#'   uniq_var      = BMIBL,
#'   label         = "BMI (kg/m^2)",
#'   sec_ord       = 1
#' )
#'
#' # Example 2: Forcing precision to 2 decimal places
#' mean_by(
#'   data          = df,
#'   group_var     = TRTAN,
#'   uniq_var      = BMIBL,
#'   label         = "BMI (kg/m^2)",
#'   sec_ord       = 1,
#'   precision_override = 2
#' )
#'
#' # Example 3: Using SAS-style rounding
#' mean_by(
#'   data          = df,
#'   group_var     = TRTAN,
#'   uniq_var      = BMIBL,
#'   label         = "BMI (kg/m^2)",
#'   sec_ord       = 1,
#'   use_sas_round = TRUE
#' )
#'
#' # Example 4: Missing treatment group automatically filled
#' df2 <- tibble::tibble(
#'   USUBJID = c(1, 2, 3, 4),
#'   TRTAN   = c(1, 1, 3, 3),
#'   BMIBL   = c(25.1, 26.3, 23.4, 27.6)
#' )
#' mean_by(
#'   data      = df2,
#'   group_var = TRTAN,
#'   uniq_var  = BMIBL,
#'   label     = "BMI (kg/m^2)",
#'   sec_ord   = 1
#' )
#'
#' @importFrom stats median quantile
#' @importFrom tidyselect any_of all_of
#' @export
mean_by <- function(
    data,
    group_var,
    uniq_var,
    label,
    sec_ord,
    precision_override = NULL,
    indent = 3,
    use_sas_round = FALSE,
    id_var = "USUBJID"
) {
  group_var <- rlang::enquo(group_var)
  uniq_var  <- rlang::enquo(uniq_var)

  # --- SAS-style rounding (local helper to avoid duplicate global function) ---
  sas_round_local <- function(x, digits = 0) {
    factor <- 10^digits
    z <- x * factor
    eps <- 1e-8
    frac <- z - trunc(z)
    is_pos_tie <- abs(frac - 0.5) < eps
    is_neg_tie <- abs(frac + 0.5) < eps
    out <- ifelse(is_pos_tie | is_neg_tie, trunc(z) + sign(z), round(z))
    out / factor
  }
  .rnd <- function(v, d) if (use_sas_round) sas_round_local(v, d) else round(v, d)
  # ---------------------------------------------------------------------------

  # Auto-detect ID variable if not present
  if (!id_var %in% names(data)) {
    id_candidates <- c("usubjid", "SUBJID", "Subject_ID", "subject_id", "subjid", "ID")
    found <- id_candidates[tolower(id_candidates) %in% tolower(names(data))]
    if (length(found) > 0) {
      id_var <- names(data)[tolower(names(data)) == tolower(found[1])]
      message("Auto-detected ID variable: ", id_var)
    } else {
      stop("Could not find ID variable `", id_var, "` or common alternatives in data.")
    }
  }
  id_var_sym <- rlang::sym(id_var)

  # Helper to infer precision from character representation
  get_precision <- function(x) {
    xs <- as.character(x)
    decimals <- vapply(xs, function(str) {
      parts <- strsplit(str, "\\.", fixed = TRUE)[[1]]
      if (length(parts) == 2) nchar(parts[2]) else 0L
    }, integer(1))
    if (all(is.na(decimals))) 0L else max(decimals, na.rm = TRUE)
  }

  # Decide precision
  precision <- if (!is.null(precision_override)) {
    if (!is.numeric(precision_override) || precision_override < 0) {
      stop("`precision_override` must be a non-negative number.")
    }
    as.integer(precision_override)
  } else {
    get_precision(data[[rlang::quo_name(uniq_var)]])
  }

  # Summary stats with conditional rounding
  result <- data %>%
    dplyr::filter(!is.na(!!uniq_var)) %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::summarise(
      N       = dplyr::n_distinct(!!id_var_sym),
      mean_   = .rnd(mean(!!uniq_var, na.rm = TRUE), precision + 1),
      sd_     = .rnd(sd(!!uniq_var,   na.rm = TRUE), precision + 2),
      Q1      = .rnd(quantile(!!uniq_var, probs = 0.25, na.rm = TRUE, type = 2), precision + 1),
      median_ = if (use_sas_round)
        sas_round_local(median(!!uniq_var, na.rm = TRUE), precision + 1)
      else
        floor(median(!!uniq_var, na.rm = TRUE) * 10^(precision + 1) + 0.5) / 10^(precision + 1),
      Q3      = .rnd(quantile(!!uniq_var, probs = 0.75, na.rm = TRUE, type = 2), precision + 1),
      min_    = .rnd(min(!!uniq_var, na.rm = TRUE), precision),
      max_    = .rnd(max(!!uniq_var, na.rm = TRUE), precision),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mn    = sprintf(paste0("%.", precision + 1, "f"), mean_),
      sd    = sprintf(paste0("%.", precision + 2, "f"), sd_),
      mn_mx = paste0(
        sprintf(paste0("%.", precision, "f"), min_), ", ",
        sprintf(paste0("%.", precision, "f"), max_)
      ),
      Q1_Q3 = paste0(
        sprintf(paste0("%.", precision + 1, "f"), Q1), ", ",
        sprintf(paste0("%.", precision + 1, "f"), Q3)
      ),
      n1    = as.character(N),
      med1  = sprintf(paste0("%.", precision + 1, "f"), median_)
    ) %>%
    dplyr::select(!!group_var, n1, mn, sd, med1, Q1_Q3, mn_mx) %>%
    tidyr::pivot_longer(
      cols           = c(n1, mn, sd, med1, Q1_Q3, mn_mx),
      names_to       = "stats",
      values_to      = "value",
      values_drop_na = TRUE
    ) %>%
    tidyr::pivot_wider(
      names_from  = !!group_var,
      values_from = value
    ) %>%
    dplyr::mutate(
      stat = dplyr::case_when(
        stats == "n1"    ~ paste0(strrep(" ", indent), "N"),
        stats == "mn"    ~ paste0(strrep(" ", indent), "MEAN"),
        stats == "sd"    ~ paste0(strrep(" ", indent), "SD"),
        stats == "med1"  ~ paste0(strrep(" ", indent), "MEDIAN"),
        stats == "Q1_Q3" ~ paste0(strrep(" ", indent), "Q1, Q3"),
        stats == "mn_mx" ~ paste0(strrep(" ", indent), "MIN, MAX")
      )
    )

  # Prepend label row & add ordering
  first_row <- tibble::tibble(stats = label, stat = label)
  result01  <- dplyr::bind_rows(first_row, result) %>%
    dplyr::mutate(sort_ord = dplyr::row_number(), sec_ord = sec_ord)

  # Prefix numeric colnames starting with digit
  colnames(result01) <- gsub("^(\\d+)", "trt\\1", colnames(result01))

  # Ensure all treatment columns exist
  trt_values <- data %>% dplyr::pull(!!group_var) %>% unique() %>% as.character()
  trt_cols   <- paste0("trt", trt_values)
  missing_cols <- setdiff(trt_cols, names(result01))

  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      result01[[col]] <- "0"
    }
  }

  # Reorder: stat first, then TRT cols
  stat_and_sort <- c("stats", "stat", "sort_ord", "sec_ord")
  result01 <- result01 %>%
    dplyr::select(any_of(stat_and_sort), all_of(sort(setdiff(names(result01), stat_and_sort))))

  return(result01)
}
