#' SOC → PT summary by treatment (wide), with optional BY-grouping, SOC totals, UNCODED positioning, BY-specific Big-N, and optional Big-N printing
#'
#' @description
#' Build a System Organ Class (SOC) → Preferred Term (PT) summary by treatment in a wide
#' layout suitable for clinical TLFs. Optionally stratify the display by a BY variable
#' from the AE dataset, order BY groups by a separate key, add TOTAL rows, control
#' UNCODED placement, and optionally calculate percentages using BY-specific denominators.
#'
#' @param indata AE-like input with at least: subject id, SOC, PT, and the main treatment column.
#'   If BY is used, `by_var` (and `by_sort_var` if different) must exist in `indata`.
#' @param dmdata Working denominator dataset (e.g., filtered ADSL) with at least: subject id and the main treatment column.
#'   If `bigN_by = "YES"` and BY is used, `dmdata` must also contain `by_var` to compute BY-specific denominators.
#' @param pop_data Master population dataset (e.g., full ADSL) used to define the set/order of treatment arms.
#'   If `NULL`, defaults to `dmdata`.
#' @param group_vars Character vector of length 3: `c(main_treatment, SOC, PT)`.
#' @param trtan_coln Treatment level value (e.g., `"12"` or `12`) that drives sorting (descending count, then alpha).
#' @param by_var Optional BY column name (quoted or unquoted) from `indata` used to split the table into groups.
#' @param by_sort_var Optional column (quoted or unquoted) used to order BY groups. Defaults to `by_var`.
#' @param by_sort_numeric If `TRUE`, BY groups ordered by `as.numeric(by_sort_var)`; else lexicographic.
#' @param id_var Subject identifier column name. Default `"USUBJID"`.
#' @param rtf_safe If `TRUE`, PT labels are prefixed by `indent_str`. Default `TRUE`.
#' @param indent_str Prefix added to PT labels when `rtf_safe = TRUE`.
#' @param use_sas_round If `TRUE`, use SAS-style rounding (ties away from zero). Default `FALSE`.
#' @param header_blank If `TRUE`, blank treatment cells on SOC header rows (TOTAL rows remain populated). Default `FALSE`.
#' @param soc_totals If `TRUE`, SOC header rows are retained/populated (default behavior). Included for API parity.
#' @param total_label Label for TOTAL row(s). Default `"TOTAL SUBJECTS WITH AN EVENT"`.
#' @param uncoded_position Where to place UNCODED: `"count"` (default behavior by counts) or `"last"` (push to bottom).
#' @param bigN_by Flag controlling denominator behavior when BY is used:
#'   - `NULL` / `"NO"` (default): denominators are by treatment only (not stratified by BY)
#'   - `"YES"`: denominators are by BY × treatment (requires `by_var` in `dmdata`)
#' @param print_bigN If `TRUE`, prints denominators (Big-N) used for percent calculations to console/log.
#'
#' @return A tibble with columns:
#' - `stat`
#' - `trt*` treatment columns
#' - `sort_ord`, `sec_ord`
#' - `by_var`, `by_sort_var` (when BY used)
#'
#' @export
SOCbyPT <- function(indata,
                    dmdata,
                    pop_data = NULL,
                    group_vars,
                    trtan_coln,
                    by_var           = NULL,   # BY strata column (AE only)
                    by_sort_var      = NULL,   # optional sort-key column for BY strata
                    by_sort_numeric  = TRUE,   # if TRUE, order BY strata by as.numeric(by_sort_var)
                    id_var           = "USUBJID",
                    rtf_safe         = TRUE,
                    indent_str       = "(*ESC*)R/RTF\"\\li360 \"",
                    use_sas_round    = FALSE,
                    header_blank     = FALSE,
                    soc_totals       = FALSE,
                    total_label      = "TOTAL SUBJECTS WITH AN EVENT",
                    uncoded_position = c("count","last"),
                    bigN_by          = NULL,
                    print_bigN       = FALSE) {

  ## ---- helpers --------------------------------------------------------------
  uncoded_position <- match.arg(uncoded_position)

  .norm_uc <- function(x) {
    x <- as.character(x)
    x <- gsub("\u00A0", " ", x, fixed = TRUE)  # NBSP → space
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)
    toupper(x)
  }
  .is_uncoded <- function(x) .norm_uc(x) %in% "UNCODED"

  .as_yesno <- function(x) {
    if (is.null(x)) return(FALSE)
    if (is.logical(x)) return(isTRUE(x))
    if (is.numeric(x)) return(isTRUE(as.logical(x)))
    if (is.character(x)) return(toupper(trimws(x)) %in% c("Y","YES","TRUE","T","1"))
    FALSE
  }

  bigN_by    <- .as_yesno(bigN_by)
  print_bigN <- .as_yesno(print_bigN)

  ## ---- validations ----------------------------------------------------------
  stopifnot(length(group_vars) == 3)
  main_grp <- group_vars[1]; soc_grp <- group_vars[2]; pt_grp <- group_vars[3]

  missing_ind <- setdiff(c(id_var, group_vars), names(indata))
  if (length(missing_ind)) stop(sprintf("Columns missing in 'indata': %s", paste(missing_ind, collapse = ", ")))

  missing_dm <- setdiff(c(id_var, group_vars[1]), names(dmdata))
  if (length(missing_dm)) stop(sprintf("Required columns missing in 'dmdata': %s", paste(missing_dm, collapse = ", ")))

  if (!is.null(pop_data) && !(group_vars[1] %in% names(pop_data))) {
    stop(sprintf("Treatment column '%s' not found in 'pop_data'.", group_vars[1]))
  }

  ## ---- syms -----------------------------------------------------------------
  main_sym <- rlang::sym(main_grp); soc_sym <- rlang::sym(soc_grp)
  pt_sym   <- rlang::sym(pt_grp);   id_sym  <- rlang::sym(id_var)

  ## ---- normalize BY vars (accept quoted or unquoted) ------------------------
  if (!is.null(by_var)) {
    by_sym <- rlang::ensym(by_var); by_var <- rlang::as_name(by_sym)
    if (!(by_var %in% names(indata))) stop(sprintf("BY variable '%s' not found in 'indata'.", by_var))
  } else {
    by_sym <- NULL
  }

  if (!is.null(by_sym)) {
    if (is.null(by_sort_var)) by_sort_var <- by_var
    by_sort_sym <- rlang::ensym(by_sort_var); by_sort_var <- rlang::as_name(by_sort_sym)
    if (!(by_sort_var %in% names(indata))) stop(sprintf("by_sort_var '%s' not found in 'indata'.", by_sort_var))

    # one-to-one map from BY -> BY_SORT (if by_sort_var differs)
    by_map <- indata %>%
      dplyr::select(dplyr::all_of(c(by_var, by_sort_var))) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.by_group = FALSE) %>%
      dplyr::group_by(!!rlang::sym(by_var)) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup()
  } else {
    by_sort_sym <- NULL
    by_sort_var <- NULL
    by_map <- NULL
  }

  ## ---- population for arms --------------------------------------------------
  if (is.null(pop_data)) pop_data <- dmdata

  ## ---- rounding -------------------------------------------------------------
  if (isTRUE(use_sas_round)) {
    round_impl <- function(x, digits = 0) {
      f <- 10^digits; z <- x * f; eps <- 1e-8
      frac <- z - trunc(z)
      is_pos_tie <- abs(frac - 0.5) < eps
      is_neg_tie <- abs(frac + 0.5) < eps
      out <- ifelse(is_pos_tie | is_neg_tie, trunc(z) + sign(z), round(z))
      out / f
    }
  } else {
    round_impl <- base::round
  }
  pct_fmt <- function(n, N) sprintf("%.1f", ifelse(N > 0, round_impl(n / N * 100, 1), 0))

  ## ---- coerce trtan_coln type ----------------------------------------------
  trt_vec <- pop_data[[main_grp]]
  trtan_sort_val <- trtan_coln
  if (is.factor(trt_vec)) {
    trtan_sort_val <- as.character(trtan_coln)
  } else if (is.numeric(trt_vec)) {
    trtan_sort_val <- suppressWarnings(as.numeric(trtan_coln))
  } else {
    trtan_sort_val <- as.character(trtan_coln)
  }

  ## ---- arm levels -----------------------------------------------------------
  trt_levels <- unique(trt_vec)
  trt_levels <- if (is.numeric(trt_levels)) sort(trt_levels) else sort(as.character(trt_levels))
  res_cols   <- paste0("result_", trt_levels)

  ## ---- denominators (Big-N) -------------------------------------------------
  # Default: by treatment only
  # If bigN_by=YES and BY used: by BY × treatment (requires by_var present in dmdata)
  if (is.null(by_sym) || !isTRUE(bigN_by)) {

    tot <- dmdata %>%
      dplyr::group_by(!!main_sym) %>%
      dplyr::summarise(N = dplyr::n_distinct(!!id_sym), .groups = "drop")

    denom_join_by <- main_grp

  } else {

    if (!(by_var %in% names(dmdata))) {
      stop(sprintf(
        "bigN_by='YES' requires BY variable '%s' to exist in 'dmdata' to compute BY-specific denominators.",
        by_var
      ))
    }

    tot <- dmdata %>%
      dplyr::group_by(!!rlang::sym(by_var), !!main_sym) %>%
      dplyr::summarise(N = dplyr::n_distinct(!!id_sym), .groups = "drop")

    denom_join_by <- c(by_var, main_grp)
  }

  ## ---- optional Big-N printing ---------------------------------------------
  if (isTRUE(print_bigN)) {
    cat("\n[Big-N] Denominators used")
    if (identical(denom_join_by, main_grp)) {
      cat(" (by treatment):\n")
      for (i in seq_len(nrow(tot))) {
        cat(sprintf("  %s=%s : N=%s\n",
                    main_grp,
                    as.character(tot[[main_grp]][i]),
                    tot$N[i]))
      }
    } else {
      cat(sprintf(" (BY=%s):\n", by_var))
      tot_print <- tot %>%
        dplyr::arrange(!!rlang::sym(by_var), !!main_sym)
      for (i in seq_len(nrow(tot_print))) {
        cat(sprintf("  %s | %s=%s : N=%s\n",
                    as.character(tot_print[[by_var]][i]),
                    main_grp,
                    as.character(tot_print[[main_grp]][i]),
                    tot_print$N[i]))
      }
    }
    cat("\n")
  }

  ## ---- SOC long -------------------------------------------------------------
  soc_long <- if (is.null(by_sym)) {
    indata %>%
      dplyr::group_by(!!soc_sym, !!main_sym) %>%
      dplyr::summarise(n_ = dplyr::n_distinct(!!id_sym), .groups = "drop") %>%
      dplyr::left_join(tot, by = denom_join_by) %>%
      dplyr::mutate(result = paste0(n_, " (", pct_fmt(n_, N), ")"))
  } else {
    indata %>%
      dplyr::group_by(!!by_sym, !!soc_sym, !!main_sym) %>%
      dplyr::summarise(n_ = dplyr::n_distinct(!!id_sym), .groups = "drop") %>%
      dplyr::left_join(tot, by = denom_join_by) %>%
      dplyr::mutate(result = paste0(n_, " (", pct_fmt(n_, N), ")"))
  }

  ## ---- SOC sort helper (counts in sorting arm) ------------------------------
  soc_sort <- if (is.null(by_sym)) {
    soc_long %>%
      dplyr::filter(!!main_sym == trtan_sort_val) %>%
      dplyr::group_by(!!soc_sym) %>%
      dplyr::summarise(sort_n = sum(n_, na.rm = TRUE), .groups = "drop")
  } else {
    soc_long %>%
      dplyr::filter(!!main_sym == trtan_sort_val) %>%
      dplyr::group_by(!!by_sym, !!soc_sym) %>%
      dplyr::summarise(sort_n = sum(n_, na.rm = TRUE), .groups = "drop")
  }

  ## ---- SOC wide -------------------------------------------------------------
  if (is.null(by_sym)) {
    soc_sum <- soc_long %>%
      dplyr::select(!!soc_sym, !!main_sym, result) %>%
      tidyr::pivot_wider(names_from = !!main_sym, values_from = result, names_prefix = "result_")
  } else {
    soc_sum <- soc_long %>%
      dplyr::select(!!by_sym, !!soc_sym, !!main_sym, result) %>%
      tidyr::pivot_wider(names_from = !!main_sym, values_from = result, names_prefix = "result_") %>%
      dplyr::left_join(by_map, by = by_var)  # keep by_sort_var
  }

  # ensure all result_* cols exist
  miss_soc <- setdiff(res_cols, names(soc_sum))
  for (mc in miss_soc) soc_sum[[mc]] <- NA_character_
  if (!nrow(soc_sum)) {
    if (is.null(by_sym)) {
      soc_sum <- tibble::tibble(!!soc_sym := character())
      for (mc in res_cols) soc_sum[[mc]] <- character()
    } else {
      soc_sum <- tibble::tibble(!!by_sym := character(), !!soc_sym := character())
      soc_sum[[by_sort_var]] <- character()
      for (mc in res_cols) soc_sum[[mc]] <- character()
    }
  }

  ## ---- order SOC blocks -----------------------------------------------------
  if (is.null(by_sym)) {
    soc_sum <- soc_sum %>%
      dplyr::left_join(soc_sort, by = soc_grp) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(sort_n, 0)) %>%
      dplyr::arrange(dplyr::desc(sort_n), !!soc_sym) %>%
      dplyr::mutate(sec_ord  = dplyr::row_number(),
                    stat     = as.character(!!soc_sym),
                    sort_ord = 1L)
    soc_order <- soc_sum %>% dplyr::transmute(!!soc_sym := stat, sec_ord)
  } else {
    soc_sum <- soc_sum %>%
      dplyr::left_join(soc_sort, by = c(by_var, soc_grp)) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(sort_n, 0))

    soc_sum <- if (isTRUE(by_sort_numeric)) {
      soc_sum %>% dplyr::arrange(suppressWarnings(as.numeric(.data[[by_sort_var]])),
                                 dplyr::desc(sort_n), !!soc_sym)
    } else {
      soc_sum %>% dplyr::arrange(!!rlang::sym(by_sort_var),
                                 dplyr::desc(sort_n), !!soc_sym)
    }

    soc_sum <- soc_sum %>%
      dplyr::group_by(!!rlang::sym(by_var)) %>%
      dplyr::mutate(sec_ord = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(stat     = as.character(!!soc_sym),
                    sort_ord = 1L)

    soc_order <- soc_sum %>% dplyr::transmute(!!rlang::sym(by_var), !!soc_sym := stat, sec_ord)
  }

  ## ---- PT long --------------------------------------------------------------
  pt_long <- if (is.null(by_sym)) {
    indata %>%
      dplyr::group_by(!!soc_sym, !!pt_sym, !!main_sym) %>%
      dplyr::summarise(n_ = dplyr::n_distinct(!!id_sym), .groups = "drop") %>%
      dplyr::left_join(tot, by = denom_join_by) %>%
      dplyr::mutate(result = paste0(n_, " (", pct_fmt(n_, N), ")"))
  } else {
    indata %>%
      dplyr::group_by(!!by_sym, !!soc_sym, !!pt_sym, !!main_sym) %>%
      dplyr::summarise(n_ = dplyr::n_distinct(!!id_sym), .groups = "drop") %>%
      dplyr::left_join(tot, by = denom_join_by) %>%
      dplyr::mutate(result = paste0(n_, " (", pct_fmt(n_, N), ")"))
  }

  ## ---- PT sort helper -------------------------------------------------------
  pt_sort <- if (is.null(by_sym)) {
    pt_long %>%
      dplyr::filter(!!main_sym == trtan_sort_val) %>%
      dplyr::select(!!soc_sym, !!pt_sym, sort_n = n_)
  } else {
    pt_long %>%
      dplyr::filter(!!main_sym == trtan_sort_val) %>%
      dplyr::select(!!by_sym, !!soc_sym, !!pt_sym, sort_n = n_)
  }

  ## ---- PT wide --------------------------------------------------------------
  if (is.null(by_sym)) {
    pt_wide <- pt_long %>%
      dplyr::select(!!soc_sym, !!pt_sym, !!main_sym, result) %>%
      tidyr::pivot_wider(names_from = !!main_sym, values_from = result, names_prefix = "result_")
  } else {
    pt_wide <- pt_long %>%
      dplyr::select(!!by_sym, !!soc_sym, !!pt_sym, !!main_sym, result) %>%
      tidyr::pivot_wider(names_from = !!main_sym, values_from = result, names_prefix = "result_") %>%
      dplyr::left_join(by_map, by = by_var)  # keep by_sort_var
  }

  miss_pt <- setdiff(res_cols, names(pt_wide))
  for (mc in miss_pt) pt_wide[[mc]] <- NA_character_
  if (!nrow(pt_wide)) {
    if (is.null(by_sym)) {
      pt_wide <- tibble::tibble(!!soc_sym := character(), !!pt_sym := character())
      for (mc in res_cols) pt_wide[[mc]] <- character()
    } else {
      pt_wide <- tibble::tibble(!!by_sym := character(), !!soc_sym := character(), !!pt_sym := character())
      pt_wide[[by_sort_var]] <- character()
      for (mc in res_cols) pt_wide[[mc]] <- character()
    }
  }

  ## ---- order PT -------------------------------------------------------------
  if (is.null(by_sym)) {
    pt_wide <- pt_wide %>%
      dplyr::left_join(pt_sort, by = c(soc_grp, pt_grp)) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(sort_n, 0)) %>%
      dplyr::arrange(!!soc_sym, dplyr::desc(sort_n), !!pt_sym) %>%
      dplyr::group_by(!!soc_sym) %>%
      dplyr::mutate(sort_ord = dplyr::row_number() + 1L) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(soc_order, by = soc_grp) %>%
      dplyr::mutate(stat = if (rtf_safe) paste0(indent_str, as.character(!!pt_sym)) else as.character(!!pt_sym))
  } else {
    pt_wide <- pt_wide %>%
      dplyr::left_join(pt_sort, by = c(by_var, soc_grp, pt_grp)) %>%
      dplyr::mutate(sort_n = dplyr::coalesce(sort_n, 0))

    pt_wide <- if (isTRUE(by_sort_numeric)) {
      pt_wide %>% dplyr::arrange(suppressWarnings(as.numeric(.data[[by_sort_var]])),
                                 !!soc_sym, dplyr::desc(sort_n), !!pt_sym)
    } else {
      pt_wide %>% dplyr::arrange(!!rlang::sym(by_sort_var),
                                 !!soc_sym, dplyr::desc(sort_n), !!pt_sym)
    }

    pt_wide <- pt_wide %>%
      dplyr::group_by(!!rlang::sym(by_var), !!soc_sym) %>%
      dplyr::mutate(sort_ord = dplyr::row_number() + 1L) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(soc_order, by = c(by_var, soc_grp)) %>%
      dplyr::mutate(stat = if (rtf_safe) paste0(indent_str, as.character(!!pt_sym)) else as.character(!!pt_sym))
  }

  ## ---- stack SOC + PT -------------------------------------------------------
  out_tbl <- dplyr::bind_rows(soc_sum, pt_wide)
  names(out_tbl) <- sub("^result_", "trt", names(out_tbl))

  ## ---- TOTAL SUBJECTS WITH AN EVENT ----------------------------------------
  if (is.null(by_sym)) {

    any_ev <- indata %>%
      dplyr::distinct(!!id_sym, !!main_sym) %>%
      dplyr::count(!!main_sym, name = "n_") %>%
      dplyr::left_join(tot, by = denom_join_by) %>%
      dplyr::mutate(result = paste0(n_, " (", pct_fmt(n_, N), ")")) %>%
      dplyr::select(!!main_sym, result) %>%
      tidyr::pivot_wider(names_from = !!main_sym, values_from = result, names_prefix = "trt")

    for (cc in paste0("trt", trt_levels)) if (!cc %in% names(any_ev)) any_ev[[cc]] <- "0"

    total_row <- tibble::tibble(stat = total_label, sort_ord = 0L, sec_ord = 0L)
    for (cc in paste0("trt", trt_levels)) total_row[[cc]] <- any_ev[[cc]][1]

  } else {

    any_ev <- indata %>%
      dplyr::distinct(!!id_sym, !!rlang::sym(by_var), !!main_sym) %>%
      dplyr::count(!!rlang::sym(by_var), !!main_sym, name = "n_") %>%
      dplyr::left_join(tot, by = denom_join_by) %>%
      dplyr::mutate(result = paste0(n_, " (", pct_fmt(n_, N), ")")) %>%
      dplyr::select(!!rlang::sym(by_var), !!main_sym, result) %>%
      tidyr::pivot_wider(id_cols = !!rlang::sym(by_var),
                         names_from = !!main_sym,
                         values_from = result,
                         names_prefix = "trt") %>%
      dplyr::left_join(by_map, by = by_var)

    for (cc in paste0("trt", trt_levels)) if (!cc %in% names(any_ev)) any_ev[[cc]] <- "0"

    total_row <- any_ev %>%
      dplyr::mutate(stat = total_label, sort_ord = 0L, sec_ord = 0L)

    total_row <- if (isTRUE(by_sort_numeric)) {
      total_row %>% dplyr::arrange(suppressWarnings(as.numeric(.data[[by_sort_var]])))
    } else {
      total_row %>% dplyr::arrange(!!rlang::sym(by_sort_var))
    }
  }

  out_tbl <- dplyr::bind_rows(total_row, out_tbl)

  ## ---- UNCODED handling (count vs last) ------------------------------------
  if (is.null(by_sym)) {
    if (identical(uncoded_position, "last")) {
      out_tbl <- out_tbl %>%
        dplyr::mutate(uncoded = .is_uncoded(stat)) %>%
        dplyr::mutate(sec_ord = ifelse(uncoded, sec_ord + max(sec_ord, na.rm = TRUE), sec_ord)) %>%
        dplyr::select(-uncoded)
    }
    out_tbl <- out_tbl %>% dplyr::arrange(sec_ord, sort_ord)
  } else {
    out_tbl <- out_tbl %>% dplyr::mutate(uncoded = .is_uncoded(stat))
    if (identical(uncoded_position, "last")) {
      out_tbl <- out_tbl %>%
        dplyr::group_by(!!rlang::sym(by_var)) %>%
        dplyr::mutate(sec_ord = ifelse(uncoded, sec_ord + max(sec_ord, na.rm = TRUE), sec_ord)) %>%
        dplyr::ungroup()
    }
    out_tbl <- out_tbl %>% dplyr::select(-uncoded)

    out_tbl <- if (isTRUE(by_sort_numeric)) {
      out_tbl %>% dplyr::arrange(suppressWarnings(as.numeric(.data[[by_sort_var]])), sec_ord, sort_ord)
    } else {
      out_tbl %>% dplyr::arrange(!!rlang::sym(by_sort_var), sec_ord, sort_ord)
    }
  }

  ## ---- ensure trt* exist, fill NA -> "0" -----------------------------------
  for (cc in paste0("trt", trt_levels)) if (!cc %in% names(out_tbl)) out_tbl[[cc]] <- NA_character_
  trt_out <- grep("^trt", names(out_tbl), value = TRUE)
  if (length(trt_out)) {
    out_tbl[trt_out] <- lapply(out_tbl[trt_out], function(x) { x <- as.character(x); x[is.na(x)] <- "0"; x })
  }

  ## ---- header blanking (SOC headers only; TOTAL row remains filled) --------
  if (isTRUE(header_blank) && length(trt_out)) {
    is_header <- out_tbl$sort_ord == 1L
    for (cc in trt_out) out_tbl[[cc]][is_header] <- NA_character_
  }

  ## ---- final column order ---------------------------------------------------
  if (is.null(by_sym)) {
    out_tbl <- out_tbl %>%
      dplyr::select(stat, dplyr::all_of(paste0("trt", trt_levels)), sort_ord, sec_ord) %>%
      tibble::as_tibble()
  } else {
    keep_vars <- unique(c(by_var, by_sort_var))
    out_tbl <- out_tbl %>%
      dplyr::select(dplyr::all_of(keep_vars),
                    stat,
                    dplyr::all_of(paste0("trt", trt_levels)),
                    sort_ord, sec_ord) %>%
      tibble::as_tibble()
  }

  names(out_tbl)[names(out_tbl) == "stat"] <- "STAT"
  names(out_tbl) <- sub("^trt", "TRT", names(out_tbl))

  out_tbl
}
