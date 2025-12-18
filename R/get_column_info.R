#' Extract Column Metadata from a Data Frame
#'
#' Inspects a data frame and returns a summary of metadata for each column,
#' including column name, label, format, class/type, missingness, uniqueness,
#' and (optionally) SAS-style display for Date variables (e.g., DATE9 -> 09JUL2012).
#'
#' @param df A data.frame or tibble. The input dataset whose column metadata should be extracted.
#' @param include_attributes Logical. If TRUE, includes a list-column of full attributes (after exclusions).
#' @param exclude_attributes Character vector of attribute names to drop from the attributes list.
#' @param label_attr Character vector of attribute names to check (in order) for a label.
#' @param format_attr Character vector of attribute names to check (in order) for a format.
#' @param compute_ranges Logical. If TRUE, computes min/max for numeric and date/datetime types.
#' @param sas_date_display Logical. If TRUE, adds SAS-style display columns for Date/POSIXct.
#'
#' @return A tibble with one row per column and metadata fields.
#' \itemize{
#'   \item \strong{column}: Column name
#'   \item \strong{label}: Label attribute (if present)
#'   \item \strong{format}: Format attribute (if present; e.g., DATE9.)
#'   \item \strong{class}: Class(es)
#'   \item \strong{typeof}: Underlying storage type
#'   \item \strong{n}: Total length
#'   \item \strong{n_missing}: Number of NAs
#'   \item \strong{n_unique}: Number of unique values
#'   \item \strong{min_raw/max_raw}: Min/max as raw values (Date/numeric)
#'   \item \strong{min_disp/max_disp}: Min/max as display strings (SAS-like for dates when enabled)
#'   \item \strong{sample_disp}: First non-missing value as display string (SAS-like for dates when enabled)
#'   \item \strong{attribute_names}: Comma-separated attribute names (after exclusions)
#'   \item \strong{attributes}: List column of attributes (optional)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr
#'
#' @examples
#' \dontrun{
#'   df <- haven::read_sas("adae.sas7bdat")
#'   out <- get_column_info(df)
#'   print(out)
#' }
#' @export
get_column_info <- function(
    df,
    include_attributes = TRUE,
    exclude_attributes = c("class", "row.names"),
    label_attr  = c("label", "var.label", "labelled", "Label"),
    format_attr = c("format", "format.sas", "Format", "displayWidth"),
    compute_ranges = TRUE,
    sas_date_display = TRUE
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }

  # helper: first non-null attribute among candidates
  first_attr <- function(x, keys) {
    for (k in keys) {
      v <- attr(x, k, exact = TRUE)
      if (!is.null(v)) return(v)
    }
    NULL
  }

  # helper: safe single-string conversion
  as_chr1 <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    as.character(x)[1]
  }

  # helpers: safe min/max
  get_min <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA)
    suppressWarnings(min(x, na.rm = TRUE))
  }
  get_max <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA)
    suppressWarnings(max(x, na.rm = TRUE))
  }

  # helper: SAS-like DATE9 display for Date
  fmt_date9 <- function(d) {
    if (is.na(d)) return("")
    toupper(format(d, "%d%b%Y"))  # e.g., 09JUL2012
  }

  # helper: display formatter
  to_display <- function(x) {
    if (!sas_date_display) return("")
    if (inherits(x, "Date")) {
      if (all(is.na(x))) return("")
      v <- x[which(!is.na(x))[1]]
      return(fmt_date9(v))
    }
    if (inherits(x, c("POSIXct", "POSIXt"))) {
      if (all(is.na(x))) return("")
      v <- x[which(!is.na(x))[1]]
      # A reasonable SAS-like datetime display (not exact SAS DATETIME20. etc.)
      return(toupper(format(v, "%d%b%Y:%H:%M:%S")))
    }
    ""
  }

  tibble::tibble(
    column = names(df),
    label  = purrr::map_chr(df, ~ as_chr1(first_attr(.x, label_attr))),
    format = purrr::map_chr(df, ~ as_chr1(first_attr(.x, format_attr))),
    class  = purrr::map_chr(df, ~ paste(class(.x), collapse = ", ")),
    typeof = purrr::map_chr(df, typeof),

    n         = purrr::map_chr(df, ~ as.character(length(.x))),
    n_missing = purrr::map_chr(df, ~ as.character(sum(is.na(.x)))),
    n_unique  = purrr::map_chr(df, ~ as.character(length(unique(.x)))),

    min_raw = purrr::map(df, ~ {
      if (!compute_ranges) return(NA)
      if (inherits(.x, c("Date", "POSIXct", "POSIXt")) || is.numeric(.x) || is.integer(.x)) get_min(.x) else NA
    }),
    max_raw = purrr::map(df, ~ {
      if (!compute_ranges) return(NA)
      if (inherits(.x, c("Date", "POSIXct", "POSIXt")) || is.numeric(.x) || is.integer(.x)) get_max(.x) else NA
    }),

    min_disp = purrr::map_chr(df, ~ {
      if (!compute_ranges || !sas_date_display) return("")
      if (inherits(.x, "Date")) {
        m <- get_min(.x); if (is.na(m)) "" else fmt_date9(m)
      } else if (inherits(.x, c("POSIXct", "POSIXt"))) {
        m <- get_min(.x); if (is.na(m)) "" else toupper(format(m, "%d%b%Y:%H:%M:%S"))
      } else if (is.numeric(.x) || is.integer(.x)) {
        m <- get_min(.x); if (is.na(m)) "" else as.character(m)
      } else ""
    }),
    max_disp = purrr::map_chr(df, ~ {
      if (!compute_ranges || !sas_date_display) return("")
      if (inherits(.x, "Date")) {
        m <- get_max(.x); if (is.na(m)) "" else fmt_date9(m)
      } else if (inherits(.x, c("POSIXct", "POSIXt"))) {
        m <- get_max(.x); if (is.na(m)) "" else toupper(format(m, "%d%b%Y:%H:%M:%S"))
      } else if (is.numeric(.x) || is.integer(.x)) {
        m <- get_max(.x); if (is.na(m)) "" else as.character(m)
      } else ""
    }),

    sample_disp = purrr::map_chr(df, to_display),

    attribute_names = purrr::map_chr(df, ~ {
      paste(setdiff(names(attributes(.x)), exclude_attributes), collapse = ", ")
    }),

    attributes = if (isTRUE(include_attributes)) {
      purrr::map(df, ~ {
        a <- attributes(.x)
        a[setdiff(names(a), exclude_attributes)]
      })
    } else {
      NULL
    }
  )
}
