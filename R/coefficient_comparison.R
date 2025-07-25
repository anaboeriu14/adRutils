#' Compare Two Coefficients Using Z-Score Method
#'
#' Compares two effect estimates using their standard errors via z-score calculation.
#' This implements the method described in doi: 10.1038/s41380-019-0596-9
#'
#' @param b1 Numeric. First coefficient/effect estimate
#' @param b2 Numeric. Second coefficient/effect estimate
#' @param se1 Numeric. Standard error of first estimate
#' @param se2 Numeric. Standard error of second estimate
#' @param return_pval Logical. If TRUE, returns p-value instead of z-score
#'
#' @return Numeric z-score or p-value for testing difference between coefficients
#' @export
#'
#' @examples
#' # Get z-score
#' compare_coefs(0.5, 0.3, 0.1, 0.12)
#'
#' # Get p-value
#' compare_coefs(0.5, 0.3, 0.1, 0.12, return_pval = TRUE)
#'
compare_coefs <- function(b1, b2, se1, se2, return_pval = FALSE) {
  z <- (b1 - b2) / sqrt(se1^2 + se2^2)

  if (return_pval) {
    return(2 * (1 - pnorm(abs(z))))
  } else {
    return(z)
  }
}
