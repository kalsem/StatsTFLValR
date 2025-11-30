#' Frequency Table by Group (wide): n (%) with flexible ordering and formats
#'
#' @description
#' `freq_by()` produces a one-level frequency table by treatment (wide layout)
#' where each row is a category of `last_group` (e.g., a bucketed lab value),
#' and each treatment column shows **n (%)** using distinct subject counts.
#'
#' New: If `fmt` is **not provided** (`NULL`), labels are derived from the **unique
#' values present in `data[[last_group]]`** (post `na_to_code` mapping, if used).
#'
#' It supports:
#' - **SAS-style rounding** (`use_sas_round = TRUE`) for the percent.
#' - Format mapping via either a **named vector** or a **tibble/data.frame** with
#'   columns `value` (codes) and `raw` (labels).
#' - **Ordering** by the **numeric value** of `last_group` found in the data,
#'   or optionally the **union** of format + data codes (`include_all_fmt_levels`).
#' - Counting **NA** under a chosen code/label using `na_to_code` (e.g., code `"4"` = `"MISSING"`).
#' - Auto-detecting the subject ID column when `id_var` is not provided.
#'
#' @param data A data frame containing at least `main_group`, `last_group`, and an ID column.
#' @param denom_data Optional data frame used to derive denominators (N per treatment).
#'   Defaults to `data`.
#' @param main_group Character scalar. The treatment or grouping variable name (columns in output),
#'   e.g., `"TRTAN"`.
#' @param last_group Character scalar. The categorical **code** variable to tabulate (rows).
#'   Numeric or character are both accepted; converted to character for display/ordering.
#' @param label Character scalar. A header row displayed on top (unindented).
#' @param sec_ord Integer scalar carried through for downstream table sorting.
#' @param fmt Optional. Either:
#'   - a **named character vector** like `c("1"="<1","2"="1-<4",...)` (names = codes, values = labels), or
#'   - a **data.frame/tibble** with columns `value` (codes) and `raw` (labels), or
#'   - a **string** naming an object (in parent frame) that resolves to either of the above.
#'   If `NULL` (default), labels are derived from unique values of `data[[last_group]]`.
#' @param use_sas_round Logical; if `TRUE`, percent is rounded with SAS-compatible
#'   “round halves away from zero” via `sas_round()`. Default `FALSE`.
#' @param indent Integer number of **leading spaces** applied to all category rows
#'   (the first `label` row is not indented). Default `2`.
#' @param id_var Character; the subject identifier column. If not found in `data`,
#'   the function tries common alternatives (e.g., `USUBJID`, `SUBJID`, etc.).
#' @param include_all_fmt_levels Logical; if `TRUE` (default), the row order is built from the
#'   **union of format codes and data codes** (numeric sort). When `fmt = NULL`,
#'   this effectively reduces to observed data codes only.
#' @param na_to_code Optional character scalar (e.g., `"4"`). If supplied, NA values in
#'   `last_group` are **counted under that code** before tabulation.
#'
#' @details
#' - Counting uses `n_distinct(id_var)` within each `(main_group, last_group)` cell.
#' - Percent is `100 * n / N` where `N` = distinct subjects in `denom_data` by `main_group`.
#' - When `fmt = NULL`, both **codes** and **labels** are taken from the observed values
#'   of `last_group` (after applying `na_to_code` mapping), ordered numerically where possible.
#' - Output treatment columns are normalized to `trtXX` if original names start with digits.
#' - Missing treatment arms are added as `"0"`.
#'
#' @return
#' A tibble with:
#' - `stat` (character), `sort_ord` (integer), `sec_ord` (integer),
#' - One column per treatment arm (e.g., `trt1`, `trt2`, …), with `"n (pct)"` or `"0"`.
#'
#' @examples
#' # --- Minimal toy example: Age group, Sex, Ethnic by treatment ---
#' set.seed(1)
#'
#' toy_adsl <- tibble::tibble(
#'   USUBJID = sprintf("ID%03d", 1:60),
#'   TRTAN   = sample(c(1, 2), size = 60, replace = TRUE),
#'   AGE     = sample(18:85, size = 60, replace = TRUE),
#'   SEX     = sample(c("Male", "Female"), size = 60, replace = TRUE),
#'   ETHNIC  = sample(
#'     c("Hispanic or Latino",
#'       "Not Hispanic or Latino",
#'       "Unknown",
#'       NA_character_),
#'     size = 60, replace = TRUE
#'   )
#' ) |>
#'   dplyr::mutate(
#'     AGEGR1 = dplyr::case_when(
#'       AGE < 65            ~ "<65 years",
#'       AGE >= 65 & AGE < 75 ~ "65–<75 years",
#'       AGE >= 75           ~ "≥75 years"
#'     )
#'   )
#'
#' # Denominator dataset typically comes from ADSL
#' toy_dm <- toy_adsl |>
#'   dplyr::select(USUBJID, TRTAN)
#'
#' # --- 1) Age group by treatment (auto fmt from data) ---
#' freq_by(
#'   data       = toy_adsl,
#'   denom_data = toy_dm,
#'   main_group = "TRTAN",
#'   last_group = "AGEGR1",
#'   label      = "Age group, n (%)",
#'   sec_ord    = 1,
#'   fmt        = NULL,        # derive labels from AGEGR1 values
#'   na_to_code = NULL         # AGEGR1 NA (if any) will be dropped
#' )
#'
#' # --- 2) Sex by treatment, mapping NA to a code (fmt still auto) ---
#' freq_by(
#'   data       = toy_adsl,
#'   denom_data = toy_dm,
#'   main_group = "TRTAN",
#'   last_group = "SEX",
#'   label      = "Sex, n (%)",
#'   sec_ord    = 2,
#'   fmt        = NULL,        # auto labels: "Female", "Male", "99"
#'   na_to_code = "99"         # missing SEX counted under "99"
#' )
#'
#' # --- 3) Ethnic category with explicit fmt (named vector) ---
#' fmt_ethnic <- c(
#'   "Hispanic or Latino"         = "Hispanic or Latino",
#'   "Not Hispanic or Latino"     = "Not Hispanic or Latino",
#'   "Unknown"                    = "Unknown",
#'   "99"                         = "Missing"
#' )
#'
#' freq_by(
#'   data       = toy_adsl,
#'   denom_data = toy_dm,
#'   main_group = "TRTAN",
#'   last_group = "ETHNIC",
#'   label      = "Ethnic group, n (%)",
#'   sec_ord    = 3,
#'   fmt        = fmt_ethnic,
#'   include_all_fmt_levels = TRUE,  # show all levels from fmt_ethnic
#'   na_to_code = "99"               # NA ETHNIC mapped to code "99"
#' )
#'
#' @export
freq_by <- function(data,
                    denom_data = NULL,
                    main_group,
                    last_group,
                    label,
                    sec_ord,
                    fmt = NULL,                 # <- NOW OPTIONAL
                    use_sas_round = FALSE,
                    indent = 2,
                    id_var = "USUBJID",
                    include_all_fmt_levels = TRUE,
                    na_to_code = NULL) {

  # -- 1) Auto-detect ID variable ---------------------------------------------
  if (!id_var %in% names(data)) {
    id_candidates <- c("USUBJID","SUBJID","Subject_ID","subject_id","subjid","ID")
    found <- names(data)[tolower(names(data)) %in% tolower(id_candidates)]
    if (length(found) > 0) {
      id_var <- found[1]
      message("Auto-detected ID variable: ", id_var)
    } else {
      stop("Could not find ID variable `", id_var, "` or common alternatives in `data`.")
    }
  }
  id_sym   <- rlang::sym(id_var)
  main_sym <- rlang::sym(main_group)
  last_sym <- rlang::sym(last_group)

  # -- 2) Default denominator --------------------------------------------------
  if (is.null(denom_data)) denom_data <- data

  # -- 3) Prepare data codes & optional NA→code mapping -----------------------
  data_codes_chr <- as.character(dplyr::pull(data, !!last_sym))
  if (!is.null(na_to_code)) {
    data_codes_chr <- dplyr::coalesce(data_codes_chr, na_to_code)
    data <- data |>
      dplyr::mutate(!!last_sym := dplyr::coalesce(as.character(!!last_sym), na_to_code))
  }

  # -- 4) Resolve/construct fmt ------------------------------------------------
  fmt_named <- NULL

  if (is.null(fmt)) {
    # Build labels directly from observed data codes (after NA mapping)
    observed <- unique(data_codes_chr)
    observed <- observed[!is.na(observed) & observed != ""]
    # keep numeric order first, then alpha as fallback
    ord <- order(suppressWarnings(as.numeric(observed)), observed, na.last = TRUE)
    observed <- observed[ord]
    fmt_named <- stats::setNames(observed, observed)
  } else {
    # allow fmt = "object_name"
    if (is.character(fmt) && length(fmt) == 1) {
      fmt <- get(fmt, envir = parent.frame())
    }
    if (is.data.frame(fmt) && all(c("value","raw") %in% names(fmt))) {
      fmt_named <- fmt |>
        dplyr::mutate(value_chr = as.character(.data$value)) |>
        dplyr::filter(!is.na(.data$value_chr) & .data$value_chr != "") |>
        dplyr::transmute(name = .data$value_chr, value = .data$raw) |>
        tibble::deframe()
    } else if (is.atomic(fmt) && !is.null(names(fmt))) {
      fmt_named <- fmt
    } else {
      stop("`fmt` must be NULL, a named vector, or a data.frame with columns `value` and `raw`.")
    }
  }

  fmt_codes <- names(fmt_named)
  fmt_labs  <- unname(fmt_named)

  # -- 5) Build code set (respect include_all_fmt_levels only when fmt provided)-
  if (!is.null(fmt) && isTRUE(include_all_fmt_levels)) {
    codes_all <- unique(c(fmt_codes, data_codes_chr))
  } else {
    codes_all <- unique(data_codes_chr)
  }
  # drop NA/blank
  codes_all <- codes_all[!is.na(codes_all) & codes_all != ""]
  ord_all <- order(suppressWarnings(as.numeric(codes_all)), codes_all, na.last = TRUE)
  codes_all <- codes_all[ord_all]
  labels_all <- ifelse(codes_all %in% fmt_codes, unname(fmt_named[codes_all]), codes_all)

  # -- 6) Factor with these levels/labels -------------------------------------
  map_df <- tibble::tibble(code = codes_all, label = labels_all)
  data2 <- data |>
    dplyr::mutate(
      !!last_sym := factor(
        as.character(!!last_sym),
        levels = map_df$code,
        labels = map_df$label
      )
    )

  # -- 7) Denominators (distinct subjects per treatment) ----------------------
  denom <- denom_data |>
    dplyr::group_by(!!main_sym) |>
    dplyr::summarise(N = dplyr::n_distinct(!!id_sym), .groups = "drop")

  # -- 8) Numerators (distinct subjects per cell) -----------------------------
  num <- data2 |>
    dplyr::group_by(!!main_sym, !!last_sym) |>
    dplyr::summarise(n_ = dplyr::n_distinct(!!id_sym), .groups = "drop")

  # -- 9) Complete grid, compute %, format cells ------------------------------
  full <- num |>
    tidyr::complete(!!main_sym, !!last_sym, fill = list(n_ = 0)) |>
    dplyr::left_join(denom, by = rlang::set_names(main_group, main_group)) |>
    dplyr::mutate(
      pct    = if (use_sas_round) sas_round(n_ / N * 100, 1) else round(n_ / N * 100, 1),
      result = dplyr::if_else(n_ == 0L, "0", sprintf("%3d (%5.1f)", n_, pct))
    )

  # -- 10) Pivot to wide -------------------------------------------------------
  wide <- full |>
    dplyr::select(!!last_sym, !!main_sym, result) |>
    tidyr::pivot_wider(names_from = !!main_sym, values_from = result) |>
    dplyr::rename(stat = !!last_sym)

  # -- 11) Final order & header/indent ----------------------------------------
  desired <- c(label, labels_all)
  out <- dplyr::bind_rows(tibble::tibble(stat = label), wide) |>
    dplyr::mutate(stat = factor(.data$stat, levels = desired)) |>
    dplyr::arrange(.data$stat) |>
    dplyr::mutate(
      stat     = as.character(.data$stat),
      stat     = dplyr::if_else(dplyr::row_number() == 1L, .data$stat, paste0(strrep(" ", indent), .data$stat)),
      sort_ord = dplyr::row_number(),
      sec_ord  = sec_ord
    )

  # -- 12) Standardize trt column names; ensure presence/order -----------------
  colnames(out) <- gsub("^(\\d+)", "trt\\1", colnames(out))
  trt_values <- denom |> dplyr::pull(!!main_sym) |> unique() |> as.character()
  trt_cols   <- paste0("trt", trt_values)
  miss_cols  <- setdiff(trt_cols, names(out))
  if (length(miss_cols)) for (col in miss_cols) out[[col]] <- "0"

  base_cols <- c("stat", "sort_ord", "sec_ord")
  trt_cols_ordered <- sort(intersect(trt_cols, names(out)))
  out |>
    dplyr::select(dplyr::any_of(base_cols), dplyr::all_of(trt_cols_ordered))
}
