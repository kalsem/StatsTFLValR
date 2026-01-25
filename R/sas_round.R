#' SAS-Compatible Rounding
#'
#' Performs rounding in the same manner as SAS, where values exactly halfway between two integers
#' are always rounded away from zero. This differs from R's default rounding (IEC 60559),
#' which rounds to the nearest even number ("bankers' rounding").
#'
#' @param x A numeric vector to be rounded.
#' @param digits Integer indicating the number of decimal places to round to. Default is 0.
#'
#' @return A numeric vector with values rounded using SAS-compatible logic.
#'
#' @details
#' In SAS, values like 1.5 or -2.5 are rounded to 2 and -3 respectively. This function
#' emulates that behavior by manually adjusting and checking the fractional component
#' of the value before applying rounding.
#'
#' @examples
#' sas_round(c(1.5, 2.5, 3.5, -1.5, -2.5, -3.5))
#'
#' sas_round(c(1.25, 1.35, -1.25, -1.35), digits = 1)
#'
#' sas_round(c(1.235, 1.245, -1.235, -1.245), digits = 2)
#'
#' sas_round(c(1.2345, 1.2355), digits = 3)
#'
#' sas_round(c(1.23445, 1.23455), digits = 4)
#'
#' sas_round(c(1.234445, 1.234455), digits = 5)
#'
#' @export
sas_round <- function(x, digits = 0) {
  factor <- 10^digits
  x_adj <- x * factor
  frac_part <- x_adj - trunc(x_adj)

  rounded <- ifelse(
    abs(frac_part) == 0.5,
    trunc(x_adj) + sign(x_adj),
    round(x_adj)
  )

  rounded / factor
}
