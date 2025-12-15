#' SOC → PT summary by treatment with Grade split (wide)
#'
#' Summarises AEs by **System Organ Class (SOC)** → **Preferred Term (PT)** per
#' treatment arm and splits each arm into **Grade** buckets (1–5 + NOT REPORTED).
#' The table includes a first **TOTAL SUBJECTS WITH AN EVENT** row, optional SOC
#' subtotal rows, and RTF-safe indenting for PT lines. The SOC/PT block order can
#' be driven by a reference arm (e.g., `TRTAN = 12`) and **a specific grade** via
#' `sort_grade` (default 5).
#'
#' @section Key features:
#' - **Grades from numeric and/or character sources**: Uses `grade_num` (1–5). If
#'   a character grade column exists (e.g., `"AETOCGR"`/`"AETOXGR"`), it is
#'   cleaned and mapped, with values in `nr_char_values` treated as _Not Reported_.
#' - **NR logic**: (a) For PT rows, a subject contributes the **max numeric grade**
#'   among 1–5 (NR ignored). (b) For the top **TOTAL** row, if any PT for the
#'   subject is **NR-only** (no numeric grade), the subject contributes to
#'   **NOT REPORTED**; otherwise to their **max numeric grade**.
#' - **Ordering**: Within SOC/PT, order is determined using counts from the
#'   reference arm `trtan_coln` filtered to `sort_grade` (fallback = all grades).
#' - **BY support**: Optional `by_var` (from AE) adds strata with optional
#'   `by_sort_var` to control strata ordering (numeric or character).
#' - **SOC totals**: `soc_totals = TRUE` adds a SOC subtotal row (max-grade logic).
#' - **Denominators**: Ns are computed from `dmdata` (or `pop_data`, if provided).
#' - **Big N behavior with BY**: controlled by `bigN_by` (TRT-only vs BY×TRT).
#' - **RTF-safe indent**: PT `stat` values can be indented using `indent_str`.
#' - **SAS-style rounding**: Percentages can follow SAS “round half away from
#'   zero” via `use_sas_round = TRUE`.
#' - **UNCODED placement**: `uncoded_position = c("count","last")`. With `"last"`,
#'   the block where `SOC == "UNCODED"` is forced to the very end (per BY stratum),
#'   and within that SOC the `PT == "UNCODED"` line is forced last. Detection is
#'   case-insensitive and robust to extra spaces/non-breaking spaces.
#'
#' @param indata `data.frame`. AE-like data containing \code{USUBJID}, treatment,
#'   SOC, PT, and Grade variables.
#' @param dmdata `data.frame`. ADSL-like data containing denominators per arm
#'   (must include \code{USUBJID} and the same treatment column as in `indata`).
#' @param pop_data `data.frame` or `NULL`. Optional master population for arm Ns
#'   (defaults to `dmdata`).
#' @param group_vars Character vector of length 3: \code{c(main_trt, soc, pt)}.
#'   Example: \code{c("TRTAN","AEBODSYS","AEDECOD")}.
#' @param trtan_coln Character or numeric. The **reference treatment code** used
#'   for ordering SOC/PT blocks (e.g., `"12"`).
#' @param grade_num Character. Name of numeric grade column (default `"AETOXGRN"`).
#'   Values 1–5 are treated as valid grades; others are ignored in numeric logic.
#' @param grade_char Character or `NULL`. Optional character grade column name
#'   (e.g., `"AETOCGR"`/`"AETOXGR"`). If `NULL`, the function auto-detects
#'   `"AETOCGR"` then `"AETOXGR"` if present.
#' @param by_var Character or `NULL`. Optional BY variable (from AE dataset) to
#'   generate stratified outputs and sort independently per stratum.
#' @param by_sort_var Character or `NULL`. Optional helper column to order BY
#'   strata; defaults to `by_var` when `NULL`.
#' @param by_sort_numeric Logical. If `TRUE` (default), order BY strata by
#'   `as.numeric(by_sort_var)`, else use character order.
#' @param bigN_by Flag controlling denominator behavior when BY is used:
#'   - `NULL` / `"NO"` (default): denominators are by treatment only (not stratified by BY)
#'   - `"YES"`: denominators are by BY × treatment (requires `by_var` in `dmdata` or `pop_data`)
#' @param print_bigN If `TRUE`, prints denominators (Big-N) used for percent calculations to console/log.
#' @param id_var Character. Subject ID column (default `"USUBJID"`).
#' @param rtf_safe Logical. If `TRUE` (default), prefix PT rows with `indent_str`.
#' @param indent_str Character. The RTF literal for indentation of PT lines
#'   (default `(*ESC*)R/RTF\"\\li360 \"`).
#' @param use_sas_round Logical. If `TRUE`, use SAS-style rounding for
#'   percentages; else base R `round()`.
#' @param header_blank Logical. If `TRUE` (default) and `soc_totals = FALSE`,
#'   grade columns on SOC header rows are blanked.
#' @param soc_totals Logical. If `TRUE`, include SOC subtotal rows using the same
#'   grade logic as PT rows.
#' @param total_label Character. Label for the top row (default
#'   `"TOTAL SUBJECTS WITH AN EVENT"`).
#' @param nr_char_values Character vector. Values in `grade_char` that are
#'   considered "Not Reported". Default includes multiple NR encodings.
#' @param sort_grade Integer or character. **Grade used for ordering** within the
#'   reference arm (default `5`). Use `"NOT REPORTED"` (or any synonym in
#'   `nr_char_values`) to sort by NR instead.
#' @param debug Logical. If `TRUE`, prints debug summaries.
#' @param uncoded_position Character. One of \code{c("count","last")}. Controls
#'   the placement of the UNCODED block: `"count"` = position by counts (default);
#'   `"last"` = force `SOC == "UNCODED"` to the end (per BY stratum) and
#'   `PT == "UNCODED"` last within that SOC.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{stat}
#'   \item For each treatment and each grade bucket:
#'   \code{TRT<trt>_GRADE1}, …, \code{TRT<trt>_GRADE5}, \code{TRT<trt>_NOT_REPORTED}
#'   \item \code{sort_ord}, \code{sec_ord}
#' }
#'
#' @export
SOCbyPT_Grade <- function(indata,
                          dmdata,
                          pop_data = NULL,
                          group_vars,                # c(main_trt, soc, pt)
                          trtan_coln,                # e.g. "12" (reference arm for sorting)
                          grade_num   = "AETOXGRN",  # numeric 1–5; others treated as NR
                          grade_char  = NULL,        # e.g. "AETOCGR"/"AETOXGR"; auto-pick if NULL
                          by_var      = NULL,
                          by_sort_var = NULL,
                          by_sort_numeric = TRUE,
                          bigN_by     = NULL,        # NULL/"NO"/"YES"
                          print_bigN  = FALSE,
                          id_var      = "USUBJID",
                          rtf_safe    = TRUE,
                          indent_str  = "(*ESC*)R/RTF\"\\li360 \"",
                          use_sas_round = FALSE,
                          header_blank  = TRUE,
                          soc_totals    = FALSE,
                          total_label   = "TOTAL SUBJECTS WITH AN EVENT",
                          nr_char_values = c("NOT REPORTED","NOT_REPORTED","NOTREPORTED",
                                             "NOT REPRTED","NR","N","NA"),
                          sort_grade   = 5,          # accepts 1..5 or "NOT REPORTED"
                          debug         = FALSE,
                          uncoded_position = c("count","last")) {

  uncoded_position <- match.arg(uncoded_position)

  stopifnot(length(group_vars) == 3)
  main_grp <- group_vars[1]; soc_grp <- group_vars[2]; pt_grp <- group_vars[3]

  adae <- indata
  adsl <- dmdata
  if (is.null(pop_data)) pop_data <- adsl
  popd <- pop_data

  # --- BY header label helper (minimal formatting; SEX F/M -> FEMALE/MALE) -----
  .by_disp <- function(by_name, by_value) {
    x <- as.character(by_value)
    if (!is.null(by_name) && toupper(by_name) == "SEX") {
      ux <- toupper(trimws(x))
      x <- dplyr::case_when(
        ux == "F" ~ "FEMALE",
        ux == "M" ~ "MALE",
        TRUE      ~ x
      )
    }
    x
  }

  # --- robust UNCODED detector (NBSPs, spaces, case) --------------------------
  .norm_uc <- function(x) {
    x <- as.character(x)
    x <- gsub("\u00A0", " ", x, fixed = TRUE)  # NBSP -> space
    x <- trimws(x); x <- gsub("\\s+", " ", x)
    toupper(x)
  }
  .is_uncoded <- function(x) .norm_uc(x) %in% c("UNCODED")

  # force character for join keys; keep numeric-aware arm ordering separately
  adae[[main_grp]] <- as.character(adae[[main_grp]])
  adsl[[main_grp]] <- as.character(adsl[[main_grp]])
  popd[[main_grp]] <- as.character(popd[[main_grp]])
  trtan_sort_val   <- as.character(trtan_coln)

  main_sym <- rlang::sym(main_grp)
  soc_sym  <- rlang::sym(soc_grp)
  pt_sym   <- rlang::sym(pt_grp)
  id_sym   <- rlang::sym(id_var)
  by_sym   <- if (!is.null(by_var)) rlang::sym(by_var) else NULL

  # --- grade cleanup ----------------------------------------------------------
  if (is.null(grade_char)) {
    gcands <- c("AETOCGR","AETOXGR")
    hit <- gcands[gcands %in% names(adae)]
    grade_char <- if (length(hit)) hit[1] else NULL
  }

  adae[[grade_num]] <- suppressWarnings(as.numeric(adae[[grade_num]]))

  if (!is.null(grade_char) && grade_char %in% names(adae)) {
    adae[[grade_char]] <- as.character(adae[[grade_char]])
    gch <- toupper(trimws(adae[[grade_char]]))
    is_char_nr <- is.na(adae[[grade_char]]) | gch == "" | gch %in% toupper(nr_char_values)
    adae[[grade_char]][is_char_nr] <- "NOT REPORTED"
  } else {
    is_char_nr <- rep(FALSE, nrow(adae))
  }
  adae$`__IS_CHAR_NR__` <- is_char_nr

  adae$GR_NUM_EFF <- adae[[grade_num]]
  adae$GR_NUM_EFF[is_char_nr] <- NA_real_
  adae$GR_NUM_EFF[!(adae$GR_NUM_EFF %in% 1:5)] <- NA_real_

  # OPTIONAL: numeric with NR→6 (handy for sorting/debug; not exposed in output)
  adae$GR_NUM_EFF6 <- ifelse(adae$`__IS_CHAR_NR__`, 6,
                             ifelse(adae$GR_NUM_EFF %in% 1:5, adae$GR_NUM_EFF, NA_real_))

  # --- rounding helpers -------------------------------------------------------
  if (isTRUE(use_sas_round)) {
    round_impl <- function(x, d = 1) {
      f <- 10^d; z <- x * f; eps <- 1e-8; frac <- z - trunc(z)
      tie <- abs(frac - .5) < eps | abs(frac + .5) < eps
      (ifelse(tie, trunc(z) + sign(z), round(z))) / f
    }
  } else {
    round_impl <- function(x, d = 1) round(x, d)
  }
  pct_fmt <- function(n, N) sprintf("%.1f", ifelse(!is.na(N) & N > 0, round_impl(100 * n / N, 1), 0))

  # --- numeric-aware ordering of treatment levels -----------------------------
  trt_vals_chr <- as.character(popd[[main_grp]])
  trt_vals_num <- suppressWarnings(as.numeric(trt_vals_chr))
  ord <- order(trt_vals_num, trt_vals_chr, na.last = TRUE)
  trt_levels <- unique(trt_vals_chr[ord])

  colname_for <- function(trt, g) {
    paste0("TRT", trt, "_", ifelse(g == "NOT REPORTED", "NOT_REPORTED", paste0("GRADE", g)))
  }
  grade_buckets  <- c("1","2","3","4","5","NOT REPORTED")
  all_grade_cols <- unlist(lapply(trt_levels, function(trt) colname_for(trt, grade_buckets)))

  fill_grade_cols <- function(df) {
    for (cn in all_grade_cols) {
      if (!cn %in% names(df)) df[[cn]] <- "0"
      df[[cn]] <- as.character(df[[cn]])
      df[[cn]][is.na(df[[cn]])] <- "0"
    }
    df
  }

  # denominators (Big N) -------------------------------------------------------
  denom_df <- if (is.null(pop_data)) adsl else pop_data
  denom_df[[main_grp]] <- as.character(denom_df[[main_grp]])

  bigN_flag <- if (is.null(bigN_by)) "NO" else toupper(as.character(bigN_by))
  if (!bigN_flag %in% c("NO", "YES")) stop("bigN_by must be NULL, 'NO', or 'YES'.", call. = FALSE)

  if (identical(bigN_flag, "YES")) {
    if (is.null(by_var)) stop("bigN_by='YES' requires by_var to be provided.", call. = FALSE)
    if (!by_var %in% names(denom_df)) {
      stop("bigN_by='YES' requires by_var to exist in dmdata/pop_data used for denominators.", call. = FALSE)
    }
    tot <- denom_df %>%
      dplyr::group_by(!!main_sym, !!by_sym) %>%
      dplyr::summarise(N = dplyr::n_distinct(!!id_sym), .groups = "drop")
    join_keys <- c(main_grp, by_var)
  } else {
    tot <- denom_df %>%
      dplyr::group_by(!!main_sym) %>%
      dplyr::summarise(N = dplyr::n_distinct(!!id_sym), .groups = "drop")
    join_keys <- main_grp
  }

  if (isTRUE(print_bigN)) {
    message("Big-N denominators used for % calculations:")
    if (identical(bigN_flag, "YES")) {
      print(
        tot %>%
          dplyr::arrange(.data[[by_var]], .data[[main_grp]]) %>%
          dplyr::rename(BY = !!by_sym, TRT = !!main_sym)
      )
    } else {
      print(
        tot %>%
          dplyr::arrange(.data[[main_grp]]) %>%
          dplyr::rename(TRT = !!main_sym)
      )
    }
  }

  # ---- PT rows: worst grade with NR-first (NR→6) -----------------------------
  grp_pt <- c(id_var, main_grp, soc_grp, pt_grp)
  if (!is.null(by_var)) grp_pt <- c(grp_pt, by_var)

  subj_pt <- adae %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_pt))) %>%
    dplyr::summarise(
      ANY_NR  = any(.data$`__IS_CHAR_NR__`, na.rm = TRUE),
      HAS_NUM = any(.data$GR_NUM_EFF %in% 1:5, na.rm = TRUE),
      MAX_NUM = ifelse(HAS_NUM, suppressWarnings(max(.data$GR_NUM_EFF, na.rm = TRUE)), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      WORST_NUM = dplyr::case_when(
        .data$ANY_NR  ~ 6,
        .data$HAS_NUM ~ .data$MAX_NUM,
        TRUE          ~ 6
      ),
      GRADE_LAB = dplyr::case_when(
        .data$WORST_NUM == 6 ~ "NOT_REPORTED",
        TRUE                 ~ paste0("GRADE", as.integer(.data$WORST_NUM))
      )
    )

  # ---- optional SOC totals (NR-first) ----------------------------------------
  if (isTRUE(soc_totals)) {
    grp_soc <- c(id_var, main_grp, soc_grp)
    if (!is.null(by_var)) grp_soc <- c(grp_soc, by_var)

    soc_long <- adae %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_soc))) %>%
      dplyr::summarise(
        ANY_NR  = any(.data$`__IS_CHAR_NR__`, na.rm = TRUE),
        HAS_NUM = any(.data$GR_NUM_EFF %in% 1:5, na.rm = TRUE),
        MAX_NUM = ifelse(HAS_NUM, suppressWarnings(max(.data$GR_NUM_EFF, na.rm = TRUE)), NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        WORST_NUM = dplyr::case_when(
          .data$ANY_NR  ~ 6,
          .data$HAS_NUM ~ .data$MAX_NUM,
          TRUE          ~ 6
        ),
        GRADE_LAB = dplyr::case_when(
          .data$WORST_NUM == 6 ~ "NOT_REPORTED",
          TRUE                 ~ paste0("GRADE", as.integer(.data$WORST_NUM))
        )
      ) %>%
      dplyr::count(dplyr::across(dplyr::all_of(setdiff(grp_soc, id_var))), GRADE_LAB, name = "n_") %>%
      dplyr::left_join(tot, by = join_keys) %>%
      dplyr::mutate(result = paste0(.data$n_, " (", pct_fmt(.data$n_, .data$N), ")")) %>%
      dplyr::select(dplyr::all_of(setdiff(grp_soc, id_var)), "GRADE_LAB", "result") %>%
      tidyr::pivot_wider(
        names_from   = c(!!main_sym, GRADE_LAB),
        values_from  = result,
        names_sep    = "_",
        names_prefix = "TRT"
      )
  }

  # ---- TOTAL row logic (NR-first across ALL AEs for the subject) -------------
  grp_all <- c(id_var, main_grp)
  if (!is.null(by_var)) grp_all <- c(grp_all, by_var)

  subj_all <- adae %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_all))) %>%
    dplyr::summarise(
      ANY_NR  = any(.data$`__IS_CHAR_NR__`, na.rm = TRUE),
      HAS_NUM = any(.data$GR_NUM_EFF %in% 1:5, na.rm = TRUE),
      MAX_NUM = ifelse(HAS_NUM, suppressWarnings(max(.data$GR_NUM_EFF, na.rm = TRUE)), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      WORST_NUM = dplyr::case_when(
        .data$ANY_NR  ~ 6,
        .data$HAS_NUM ~ .data$MAX_NUM,
        TRUE          ~ 6
      ),
      GRADE_LAB = dplyr::case_when(
        .data$WORST_NUM == 6 ~ "NOT_REPORTED",
        TRUE                 ~ paste0("GRADE", as.integer(.data$WORST_NUM))
      )
    )

  if (debug) {
    message("DEBUG: nrow(ADAE)=", nrow(adae),
            " | distinct subjects=", dplyr::n_distinct(adae[[id_var]]))
    message("DEBUG: subj_pt rows=", nrow(subj_pt), " | subj_all rows=", nrow(subj_all))
    dbg <- subj_all %>%
      dplyr::count(!!main_sym, GRADE_LAB, name = "n") %>%
      tidyr::complete(!!main_sym,
                      GRADE_LAB = c("GRADE1","GRADE2","GRADE3","GRADE4","GRADE5","NOT_REPORTED"),
                      fill = list(n = 0)) %>%
      tidyr::pivot_wider(names_from = GRADE_LAB, values_from = n, values_fill = 0)
    message("DEBUG: TOTAL grade split (by arm):"); print(dbg)
  }

  # ---- PT counts -> wide -----------------------------------------------------
  if (is.null(by_var)) {
    pt_long <- subj_pt %>% dplyr::count(!!soc_sym, !!pt_sym, !!main_sym, GRADE_LAB, name = "n_")
  } else {
    pt_long <- subj_pt %>% dplyr::count(!!by_sym, !!soc_sym, !!pt_sym, !!main_sym, GRADE_LAB, name = "n_")
  }

  join_keys_pt <- join_keys
  pt_select_vars <- c(soc_grp, pt_grp, main_grp)
  if (!is.null(by_var)) pt_select_vars <- c(by_var, pt_select_vars)

  pt_wide <- pt_long %>%
    dplyr::left_join(tot, by = join_keys_pt) %>%
    dplyr::mutate(result = paste0(.data$n_, " (", pct_fmt(.data$n_, .data$N), ")")) %>%
    dplyr::select(dplyr::all_of(pt_select_vars), "GRADE_LAB", "result") %>%
    tidyr::pivot_wider(
      names_from   = c(!!main_sym, GRADE_LAB),
      values_from  = result,
      names_sep    = "_",
      names_prefix = "TRT"
    )
  pt_wide <- fill_grade_cols(pt_wide)

  # ---- SOC headers (labels + optional totals) --------------------------------
  if (is.null(by_var)) {
    soc_wide <- adae %>% dplyr::distinct(!!soc_sym) %>% dplyr::arrange(!!soc_sym)
  } else {
    soc_wide <- adae %>% dplyr::distinct(!!by_sym, !!soc_sym)
    if (is.null(by_sort_var)) by_sort_var <- by_var
    if (by_sort_numeric) {
      soc_wide <- soc_wide %>%
        dplyr::arrange(suppressWarnings(as.numeric(.data[[by_sort_var]])), !!soc_sym)
    } else {
      soc_wide <- soc_wide %>% dplyr::arrange(.data[[by_sort_var]], !!soc_sym)
    }
  }

  soc_wide$`_SUBVARLBL_` <- as.character(soc_wide[[soc_grp]])
  soc_wide$sort_ord <- 1L

  if (isTRUE(soc_totals)) {
    if (is.null(by_var)) soc_wide <- dplyr::left_join(soc_wide, soc_long, by = soc_grp)
    else                 soc_wide <- dplyr::left_join(soc_wide, soc_long, by = c(by_var, soc_grp))
  } else {
    soc_wide <- fill_grade_cols(soc_wide)
  }

  # ---- ORDERING + PT labels (grade-aware) ------------------------------------
  sort_grade_lab <- NULL
  if (!is.null(sort_grade)) {
    if (is.character(sort_grade) &&
        toupper(sort_grade) %in% toupper(c("NOT REPORTED","NOT_REPORTED","NOTREPORTED","NR","N","NA"))) {
      sort_grade_lab <- "NOT_REPORTED"
    } else {
      sort_grade_lab <- paste0("GRADE", as.integer(sort_grade))
    }
  }

  if (is.null(by_var)) {

    pt_sort_ref <- subj_pt %>% dplyr::filter(.data[[main_grp]] == trtan_sort_val)
    if (!is.null(sort_grade_lab)) {
      pt_sort_ref <- pt_sort_ref %>% dplyr::filter(.data$GRADE_LAB == sort_grade_lab)
    }
    pt_sort_ref <- pt_sort_ref %>% dplyr::count(!!soc_sym, !!pt_sym, name = "sort_n")

    if (nrow(pt_sort_ref) == 0L) {
      pt_sort_ref <- subj_pt %>%
        dplyr::filter(.data[[main_grp]] == trtan_sort_val) %>%
        dplyr::count(!!soc_sym, !!pt_sym, name = "sort_n")
    }

    pt_sort_all <- adae %>%
      dplyr::distinct(!!soc_sym, !!pt_sym) %>%
      dplyr::left_join(pt_sort_ref, by = c(soc_grp, pt_grp)) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(.data$sort_n, 0L))

    soc_order <- pt_sort_all %>%
      dplyr::group_by(!!soc_sym) %>%
      dplyr::summarise(sort_n = sum(.data$sort_n, na.rm = TRUE), .groups = "drop") %>%
      dplyr::right_join(adae %>% dplyr::distinct(!!soc_sym), by = soc_grp) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(.data$sort_n, 0L)) %>%
      dplyr::arrange(dplyr::desc(.data$sort_n), !!soc_sym) %>%
      dplyr::mutate(sec_ord = dplyr::row_number()) %>%
      dplyr::select(!!soc_sym, sec_ord)

    if (identical(uncoded_position, "last")) {
      max_sec <- max(soc_order$sec_ord, na.rm = TRUE)
      soc_order <- soc_order %>%
        dplyr::mutate(sec_ord = ifelse(.is_uncoded(.data[[soc_grp]]), max_sec + 1L, .data$sec_ord))
    }

    pt_order <- pt_sort_all %>%
      dplyr::group_by(!!soc_sym) %>%
      dplyr::arrange(dplyr::desc(.data$sort_n), !!pt_sym, .by_group = TRUE) %>%
      dplyr::mutate(sort_ord = dplyr::row_number() + 1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!soc_sym, !!pt_sym, sort_ord)

    if (identical(uncoded_position, "last")) {
      pt_order <- pt_order %>%
        dplyr::group_by(!!soc_sym) %>%
        dplyr::mutate(
          sort_ord = ifelse(.is_uncoded(.data[[pt_grp]]),
                            max(.data$sort_ord, na.rm = TRUE) + 1L,
                            .data$sort_ord)
        ) %>%
        dplyr::ungroup()
    }

    soc_wide <- soc_wide %>% dplyr::left_join(soc_order, by = soc_grp)

    pt_wide  <- pt_wide %>%
      dplyr::left_join(soc_order, by = soc_grp) %>%
      dplyr::left_join(pt_order,  by = c(soc_grp, pt_grp)) %>%
      dplyr::group_by(.data[[soc_grp]]) %>%
      dplyr::mutate(
        sort_ord = ifelse(is.na(.data$sort_ord), dplyr::row_number() + 1L, .data$sort_ord),
        `_SUBVARLBL_` = if (rtf_safe) paste0(indent_str, as.character(.data[[pt_grp]]))
        else as.character(.data[[pt_grp]])
      ) %>%
      dplyr::ungroup()

  } else {

    pt_sort_ref <- subj_pt %>% dplyr::filter(.data[[main_grp]] == trtan_sort_val)
    if (!is.null(sort_grade_lab)) {
      pt_sort_ref <- pt_sort_ref %>% dplyr::filter(.data$GRADE_LAB == sort_grade_lab)
    }
    pt_sort_ref <- pt_sort_ref %>%
      dplyr::count(!!by_sym, !!soc_sym, !!pt_sym, name = "sort_n")

    if (nrow(pt_sort_ref) == 0L) {
      pt_sort_ref <- subj_pt %>%
        dplyr::filter(.data[[main_grp]] == trtan_sort_val) %>%
        dplyr::count(!!by_sym, !!soc_sym, !!pt_sym, name = "sort_n")
    }

    pt_sort_all <- adae %>%
      dplyr::distinct(!!by_sym, !!soc_sym, !!pt_sym) %>%
      dplyr::left_join(pt_sort_ref, by = c(by_var, soc_grp, pt_grp)) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(.data$sort_n, 0L))

    soc_order <- pt_sort_all %>%
      dplyr::group_by(!!by_sym, !!soc_sym) %>%
      dplyr::summarise(sort_n = sum(.data$sort_n, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(!!by_sym) %>%
      dplyr::arrange(dplyr::desc(.data$sort_n), !!soc_sym, .by_group = TRUE) %>%
      dplyr::mutate(sec_ord = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!by_sym, !!soc_sym, sec_ord)

    if (identical(uncoded_position, "last")) {
      soc_order <- soc_order %>%
        dplyr::group_by(!!by_sym) %>%
        dplyr::mutate(
          sec_ord = ifelse(.is_uncoded(.data[[soc_grp]]),
                           max(.data$sec_ord, na.rm = TRUE) + 1L,
                           .data$sec_ord)
        ) %>%
        dplyr::ungroup()
    }

    pt_order <- pt_sort_all %>%
      dplyr::group_by(!!by_sym, !!soc_sym) %>%
      dplyr::arrange(dplyr::desc(.data$sort_n), !!pt_sym, .by_group = TRUE) %>%
      dplyr::mutate(sort_ord = dplyr::row_number() + 1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!by_sym, !!soc_sym, !!pt_sym, sort_ord)

    if (identical(uncoded_position, "last")) {
      pt_order <- pt_order %>%
        dplyr::group_by(!!by_sym, !!soc_sym) %>%
        dplyr::mutate(
          sort_ord = ifelse(.is_uncoded(.data[[pt_grp]]),
                            max(.data$sort_ord, na.rm = TRUE) + 1L,
                            .data$sort_ord)
        ) %>%
        dplyr::ungroup()
    }

    soc_wide <- soc_wide %>% dplyr::left_join(soc_order, by = c(by_var, soc_grp))

    pt_wide  <- pt_wide %>%
      dplyr::left_join(soc_order, by = c(by_var, soc_grp)) %>%
      dplyr::left_join(pt_order,  by = c(by_var, soc_grp, pt_grp)) %>%
      dplyr::group_by(!!by_sym, !!soc_sym) %>%
      dplyr::mutate(
        sort_ord = ifelse(is.na(.data$sort_ord), dplyr::row_number() + 1L, .data$sort_ord),
        `_SUBVARLBL_` = if (rtf_safe) paste0(indent_str, as.character(.data[[pt_grp]]))
        else as.character(.data[[pt_grp]])
      ) %>%
      dplyr::ungroup()
  }

  # ---- TOTAL (wide) ----------------------------------------------------------
  if (is.null(by_var)) {
    total_long <- subj_all %>% dplyr::count(!!main_sym, GRADE_LAB, name = "n_")
  } else {
    total_long <- subj_all %>% dplyr::count(!!by_sym, !!main_sym, GRADE_LAB, name = "n_")
  }

  total_select_vars <- main_grp
  if (!is.null(by_var)) total_select_vars <- c(by_var, total_select_vars)

  total_wide <- total_long %>%
    dplyr::left_join(tot, by = join_keys) %>%
    dplyr::mutate(result = paste0(.data$n_, " (", pct_fmt(.data$n_, .data$N), ")")) %>%
    dplyr::select(dplyr::all_of(total_select_vars), "GRADE_LAB", "result") %>%
    tidyr::pivot_wider(
      names_from   = c(!!main_sym, GRADE_LAB),
      values_from  = result,
      names_sep    = "_",
      names_prefix = "TRT"
    )
  total_wide <- fill_grade_cols(total_wide)
  total_wide$`_SUBVARLBL_` <- total_label
  total_wide$sort_ord <- 0L
  total_wide$sec_ord  <- 0L

  # ---- stack (NO BY HEADER ROWS) ---------------------------------------------
  out_tbl <- dplyr::bind_rows(total_wide, soc_wide, pt_wide)
  out_tbl <- fill_grade_cols(out_tbl)

  if (isTRUE(header_blank) && !isTRUE(soc_totals)) {
    grade_cols <- grep("^TRT\\d+_(GRADE[1-5]|NOT_REPORTED)$", names(out_tbl), value = TRUE)
    is_soc_header <- out_tbl$sort_ord == 1L & out_tbl$`_SUBVARLBL_` != total_label
    out_tbl[is_soc_header, grade_cols] <- NA_character_
  }

  # final sort + keep BY column if present
  if (is.null(by_var)) {
    out_tbl <- out_tbl %>%
      dplyr::arrange(.data$sec_ord, .data$sort_ord) %>%
      dplyr::select(`_SUBVARLBL_`, dplyr::all_of(all_grade_cols), "sort_ord", "sec_ord") %>%
      tibble::as_tibble()
  } else {
    out_tbl <- out_tbl %>%
      dplyr::arrange(.data[[by_var]], .data$sec_ord, .data$sort_ord) %>%
      dplyr::select(dplyr::all_of(by_var), `_SUBVARLBL_`,
                    dplyr::all_of(all_grade_cols), "sort_ord", "sec_ord") %>%
      tibble::as_tibble()
  }

  if (debug) message("DEBUG: TOTAL rows in output = ", sum(out_tbl$`_SUBVARLBL_` %in% total_label, na.rm = TRUE))
  out_tbl
}
