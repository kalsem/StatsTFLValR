#' Fully Nested ATC2 → ATC4 → Drug (CMDECOD) Table by Treatment (wide)
#'
#' @description
#' Builds a three-level nested summary table of concomitant medications (or similar data),
#' grouped as **ATC2 → ATC4 → Drug (CMDECOD)**, with counts and percentages by treatment arm.
#' Outputs a **wide** data frame where each treatment column contains `n (pct)`.
#'
#' Two indent modes are supported for the display label column `stat`:
#'
#' - **RTF mode (default):** If `atc4_spaces` and `cmdecod_spaces` are both `NULL`,
#'   and `rtf_safe = TRUE`, `stat` will include the provided RTF indent strings
#'   (`atc4_rtf`, `cmdecod_rtf`) before the label text.
#' - **SAS blanks mode:** If `atc4_spaces` or `cmdecod_spaces` is provided (non-`NULL`),
#'   `stat` will use **only blank spaces** (no RTF codes) as visual indents (SAS-style),
#'   regardless of `rtf_safe`.
#'
#' Sorting can be controlled by `sort_by`:
#' - `"count"` (default): within each level, sort descending by counts for the column
#'   `n__<trtan_coln>` (e.g., `n__21`), then alphabetically.
#' - `"alpha"`: alphabetical ascending order at each level.
#'
#' Rows where **all three levels** are `"UNCODED"` (case-insensitive) are pushed to
#' the very end of the table (after all other rows), preserving the nested order.
#'
#' @param indata A data frame containing medication/event records. Must include:
#'   `USUBJID` and the variables named in `group_vars`.
#' @param dmdata A data frame with one row per subject (for denominators). Must include
#'   `USUBJID` and the main treatment grouping variable (first element of `group_vars`).
#' @param group_vars Character vector of length 4 specifying, in order:
#'   `c(main_group, atc2, atc4, meddecod)`.
#'   - `main_group` = treatment/grouping variable used for columns (e.g., `"TRTAN"`).
#'   - `atc2`, `atc4`, `meddecod` = the three nested display levels.
#' @param trtan_coln Character scalar giving the **column-level** of interest used
#'   for count-based sorting, i.e., the suffix in `n__<trtan_coln>`. Example: `"21"`
#'   makes the function look for `n__21` to drive `"count"` sorting.
#' @param rtf_safe Logical; if `TRUE`, RTF strings will be used in `stat` **when**
#'   both `atc4_spaces` and `cmdecod_spaces` are `NULL`. If either spaces
#'   argument is provided, `stat` will **not** include RTF strings.
#' @param sort_by One of `c("count","alpha")`. See Details.
#' @param atc4_spaces,cmdecod_spaces `NULL` or non-negative integer specifying the
#'   number of **blank spaces** to prepend for `ATC4` and `Drug (CMDECOD)` labels in `stat`.
#'   If either is non-`NULL`, the function uses **SAS blanks mode** (no RTF codes).
#' @param atc4_rtf,cmdecod_rtf Character RTF indent strings used **only** when
#'   *both* `atc4_spaces` and `cmdecod_spaces` are `NULL` and `rtf_safe = TRUE`.
#'   Defaults: `(*ESC*)R/RTF"\\li180 "` for `ATC4`, `(*ESC*)R/RTF"\\li360 "` for `Drug (CMDECOD)`.
#'
#' @details
#' **Denominator (`N`)** is computed from `dmdata` as distinct `USUBJID` per `main_group`.
#' For each level (ATC2, ATC4 within ATC2, Drug/CMDECOD within ATC4), the function computes
#' distinct-subject counts by `main_group`, the percentage w.r.t. `N`, and forms
#' `"n (pct)"`. The wide result has:
#'
#' - `stat` = display label with indent (RTF or blanks, depending on mode).
#' - `trt<value>` columns (e.g., `trt21`, `trt22`, …): `"n (pct)"` per treatment value.
#' - `n__<value>` columns mirroring raw counts (useful for custom sorting or QC).
#' - Ordering columns: `sec_ord`, `psec_ord`, `sort_ord` (help keep nested order).
#'
#' **Indent modes**:
#' - *RTF mode*: Use when you want RTF control words in the output for direct
#'   RTF rendering. Do **not** set `atc4_spaces`/`cmdecod_spaces`; keep `rtf_safe = TRUE`.
#' - *SAS blanks mode*: Provide `atc4_spaces` and/or `cmdecod_spaces` to indent using
#'   blanks only (friendly for plain-text outputs or RTF pipelines that inject
#'   formatting later).
#'
#' **UNCODED handling**:
#' Rows are considered UNCODED **only if** all three of `ATC2`, `ATC4`, and `Drug (CMDECOD)`
#' equal `"UNCODED"` (case-insensitive, leading/trailing space ignored). Such rows are
#' assigned to the end of the table after sorting.
#'
#' @return
#' A tibble with nested rows containing:
#' - `stat` (indented label),
#' - treatment columns `trt*` (string `"n (pct)"`),
#' - raw-count columns `n__*`,
#' - helper ordering columns (`sec_ord`, `psec_ord`, `sort_ord`).
#'
#' @export
ATCbyDrug <- function(indata, dmdata, group_vars, trtan_coln,
                      rtf_safe = TRUE,
                      sort_by = c("count", "alpha"),
                      # If either of these is provided (not NULL) → use spaces (no RTF)
                      atc4_spaces    = NULL,
                      cmdecod_spaces = NULL,
                      # RTF indents used only when no *_spaces are provided
                      atc4_rtf    = "(*ESC*)R/RTF\"\\li180 \"",
                      cmdecod_rtf = "(*ESC*)R/RTF\"\\li360 \"") {

  sort_by <- match.arg(sort_by)

  main_grp <- group_vars[1]
  atc2_grp <- group_vars[2]
  atc4_grp <- group_vars[3]
  meddecod <- group_vars[4]

  sym_main <- rlang::sym(main_grp)
  sym_atc2 <- rlang::sym(atc2_grp)
  sym_atc4 <- rlang::sym(atc4_grp)
  sym_med  <- rlang::sym(meddecod)
  usubjid  <- rlang::sym("USUBJID")

  # === helpers ===
  .spaces <- function(n) if (is.null(n) || n <= 0) "" else paste0(rep(" ", n), collapse = "")
  .sort_fun <- function(df, sort_by, sort_sym, n_col) {
    if (sort_by == "count") dplyr::arrange(df, dplyr::desc(!!rlang::sym(n_col)), !!sort_sym)
    else dplyr::arrange(df, !!sort_sym)
  }

  # toggle: if any spaces arg is set, we go "SAS blanks only" (no RTF)
  use_spaces_mode <- !is.null(atc4_spaces) || !is.null(cmdecod_spaces)

  # default missing spaces to 0 in spaces mode
  if (use_spaces_mode) {
    if (is.null(atc4_spaces))    atc4_spaces    <- 0L
    if (is.null(cmdecod_spaces)) cmdecod_spaces <- 0L
  }

  # 1) denominators
  tot <- dmdata %>%
    dplyr::group_by(!!sym_main) %>%
    dplyr::summarise(N = dplyr::n_distinct(!!usubjid), .groups = "drop")

  # 2) ATC2 (top level; no indent)
  atc2_sum <- indata %>%
    dplyr::group_by(!!sym_atc2, !!sym_main) %>%
    dplyr::summarise(n_ = dplyr::n_distinct(!!usubjid), .groups = "drop") %>%
    dplyr::left_join(tot, by = main_grp) %>%
    dplyr::mutate(
      pct    = format(round(n_ / N * 100, 1), nsmall = 1),
      result = paste0(n_, " (", pct, ")"),
      stat   = as.character(!!sym_atc2)
    ) %>%
    dplyr::select(!!sym_atc2, !!sym_main, stat, result, n_) %>%
    tidyr::pivot_wider(names_from = !!sym_main, values_from = c(result, n_))

  n_col <- paste0("n__", trtan_coln)
  if (!(n_col %in% names(atc2_sum))) atc2_sum[[n_col]] <- 0

  atc2_sum <- atc2_sum %>%
    .sort_fun(sort_by, sym_atc2, n_col) %>%
    dplyr::mutate(sec_ord = dplyr::row_number(), psec_ord = 0, sort_ord = 0)

  atc2_order <- atc2_sum %>% dplyr::select(!!sym_atc2, sec_ord)

  # 3) ATC4
  atc4_sum <- indata %>%
    dplyr::group_by(!!sym_atc2, !!sym_atc4, !!sym_main) %>%
    dplyr::summarise(n_ = dplyr::n_distinct(!!usubjid), .groups = "drop") %>%
    dplyr::left_join(tot, by = main_grp) %>%
    dplyr::mutate(
      pct  = format(round(n_ / N * 100, 1), nsmall = 1),
      stat = dplyr::case_when(
        use_spaces_mode ~ paste0(.spaces(atc4_spaces), as.character(!!sym_atc4)),
        rtf_safe        ~ paste0(atc4_rtf, as.character(!!sym_atc4)),
        TRUE            ~ as.character(!!sym_atc4)
      ),
      result = paste0(n_, " (", pct, ")")
    ) %>%
    dplyr::select(!!sym_atc2, !!sym_atc4, !!sym_main, stat, result, n_) %>%
    tidyr::pivot_wider(names_from = !!sym_main, values_from = c(result, n_))

  if (!(n_col %in% names(atc4_sum))) atc4_sum[[n_col]] <- 0

  atc4_sum <- atc4_sum %>%
    dplyr::group_by(!!sym_atc2) %>%
    .sort_fun(sort_by, sym_atc4, n_col) %>%
    dplyr::mutate(psec_ord = dplyr::row_number(), sort_ord = 0) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(atc2_order, by = atc2_grp)

  # 4) Drug / CMDECOD
  med_det <- indata %>%
    dplyr::group_by(!!sym_atc2, !!sym_atc4, !!sym_med, !!sym_main) %>%
    dplyr::summarise(n_ = dplyr::n_distinct(!!usubjid), .groups = "drop") %>%
    dplyr::left_join(tot, by = main_grp) %>%
    dplyr::mutate(
      pct  = format(round(n_ / N * 100, 1), nsmall = 1),
      stat = dplyr::case_when(
        use_spaces_mode ~ paste0(.spaces(cmdecod_spaces), as.character(!!sym_med)),
        rtf_safe        ~ paste0(cmdecod_rtf, as.character(!!sym_med)),
        TRUE            ~ as.character(!!sym_med)
      ),
      result = paste0(n_, " (", pct, ")")
    ) %>%
    dplyr::select(!!sym_atc2, !!sym_atc4, !!sym_med, !!sym_main, stat, result, n_) %>%
    tidyr::pivot_wider(names_from = !!sym_main, values_from = c(result, n_))

  if (!(n_col %in% names(med_det))) med_det[[n_col]] <- 0

  med_det <- med_det %>%
    dplyr::group_by(!!sym_atc2, !!sym_atc4) %>%
    .sort_fun(sort_by, sym_med, n_col) %>%
    dplyr::mutate(sort_ord = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      atc4_sum %>% dplyr::select(!!sym_atc2, !!sym_atc4, sec_ord, psec_ord),
      by = c(atc2_grp, atc4_grp)
    )

  # 5) bind & finalize
  final <- dplyr::bind_rows(atc2_sum, atc4_sum, med_det) %>%
    dplyr::mutate(uncoded = dplyr::if_else(
      grepl("^\\s*UNCODED\\s*$", !!sym_atc2, ignore.case = TRUE) &
        grepl("^\\s*UNCODED\\s*$", !!sym_atc4, ignore.case = TRUE) &
        grepl("^\\s*UNCODED\\s*$", !!sym_med,  ignore.case = TRUE),
      1, 0
    ))

  max_sec <- max(final$sec_ord, na.rm = TRUE)

  final <- final %>%
    dplyr::mutate(sec_ord = dplyr::if_else(uncoded == 1, max_sec + dplyr::row_number(), sec_ord)) %>%
    dplyr::arrange(sec_ord, psec_ord, sort_ord) %>%
    dplyr::select(-uncoded)

  # fill NA results with "0"
  res_cols <- grep("^result_", names(final), value = TRUE)
  final[res_cols] <- lapply(final[res_cols], function(x) ifelse(is.na(x), "0", x))

  # rename result_* -> trt*
  names(final) <- gsub("^result_", "trt", names(final))

  final
}

