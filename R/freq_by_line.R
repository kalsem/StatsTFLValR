#' One-Line Frequency Summary by Treatment Group
#'
#' Generates a single-row frequency summary table across treatment groups,
#' reporting counts and percentages of subjects meeting a filter condition.
#'
#' This function calculates the number and percentage of unique subjects per
#' treatment group (`trt_var`) satisfying a given filter condition
#' (`filter_expr`). The result is formatted as `"n (pct)"` and returned in a
#' single-row tibble, labeled by the provided `label`. An optional denominator
#' dataset (`denom_data`) can be specified to override the default denominator
#' population (used to calculate percentages).
#'
#' Useful for producing compact summary rows (e.g., "SAF Population",
#' "Subjects >= 65") in clinical tables.
#'
#' @param data A data.frame containing subject-level data.
#' @param id_var Unquoted subject ID variable (e.g., `USUBJID`).
#' @param trt_var Unquoted treatment variable (e.g., `TRT01P`).
#' @param filter_expr A logical filter expression (unquoted),
#'   e.g., `SAFFL == "Y" & AGE >= 65`.
#' @param label Character string for the row label in the output
#'   (e.g., `"SAF population"`).
#' @param denom_data Optional. A data.frame used to calculate denominators per
#'   treatment group. Defaults to `data`.
#'
#' @importFrom dplyr distinct count mutate left_join select filter %>%
#' @importFrom tidyr replace_na pivot_wider
#'
#' @return A one-row tibble containing `"n (pct)"` summaries per treatment group.
#'
#' @examples
#' set.seed(123)
#' adsl <- data.frame(
#'   USUBJID = paste0("SUBJ", 1:100),
#'   TRT01P = sample(c("0", "54", "100"), 100, replace = TRUE),
#'   SAFFL = sample(c("Y", "N"), 100, replace = TRUE),
#'   AGE = sample(18:80, 100, replace = TRUE)
#' )
#'
#' # Standard use with internal denominator
#' freq_by_line(adsl, USUBJID, TRT01P, SAFFL == "Y", label = "SAF population")
#'
#' # Use SAF subset as denominator
#' saf <- adsl[adsl$SAFFL == "Y", ]
#' freq_by_line(
#'   adsl, USUBJID, TRT01P,
#'   AGE >= 65,
#'   label = "Age >=65 in SAF",
#'   denom_data = saf
#' )
#'
#' @export
freq_by_line <- function(data, id_var, trt_var, filter_expr, label, denom_data = NULL) {
  id_var      <- rlang::ensym(id_var)
  trt_var     <- rlang::ensym(trt_var)
  filter_expr <- rlang::enquo(filter_expr)

  if (is.null(denom_data)) denom_data <- data

  # --- normalize treatment columns in BOTH data frames (type, trim) ---
  norm_trt <- function(x) {
    # keep original values for joins; only coerce to character for discovery/labels
    x_chr <- trimws(as.character(x))
    x_chr
  }

  data      <- dplyr::mutate(data,      ..TRT.. = norm_trt(!!trt_var))
  denom_data<- dplyr::mutate(denom_data,..TRT.. = norm_trt(!!trt_var))

  # Drop NA arms from the denominator when determining columns
  trt_levels_raw <- denom_data %>%
    dplyr::distinct(..TRT..) %>%
    dplyr::filter(!is.na(..TRT..) & nzchar(..TRT..)) %>%
    dplyr::pull(..TRT..)

  # Decide ordering (robust numeric-like test)
  suppressWarnings({
    trt_num <- as.numeric(trt_levels_raw)
  })
  is_numlike <- all(!is.na(trt_num)) && length(trt_num) > 0

  if (is_numlike) {
    ord <- order(trt_num, na.last = TRUE)
    trt_levels_sorted <- trt_levels_raw[ord]
    col_order <- paste0("trt", trt_levels_sorted)   # will be used after prefixing
  } else {
    trt_levels_sorted <- sort(unique(trt_levels_raw))
    col_order <- trt_levels_sorted                  # non-numeric arms, no prefix
  }

  # --- denominators (distinct id within arm) ---
  total_n <- denom_data %>%
    dplyr::distinct(!!id_var, ..TRT..) %>%
    dplyr::count(..TRT.., name = "total_n")

  # --- numerators (apply user filter on 'data') ---
  filtered_n <- data %>%
    dplyr::filter(!!filter_expr) %>%
    dplyr::distinct(!!id_var, ..TRT..) %>%
    dplyr::count(..TRT.., name = "n")

  # --- join + format ---
  result <- total_n %>%
    dplyr::left_join(filtered_n, by = "..TRT..") %>%
    dplyr::mutate(
      n   = tidyr::replace_na(n, 0L),
      pct = round(n / total_n * 100, 1),
      value = dplyr::if_else(n == 0L, "0", sprintf("%d (%.1f)", n, pct))
    ) %>%
    dplyr::select(..TRT.., value) %>%
    tidyr::pivot_wider(names_from = ..TRT.., values_from = value) %>%
    dplyr::mutate(stat = label) %>%
    dplyr::select(stat, dplyr::everything())

  # Prefix columns that begin with digits so they are syntactic names
  names(result) <- sub("^([0-9])", "trt\\1", names(result))

  # Deterministic column order
  result <- dplyr::select(result, stat, dplyr::any_of(col_order))

  return(result)
}
