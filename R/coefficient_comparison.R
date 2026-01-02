#' Compare Two Coefficients Using Z-Score Method
#'
#' Compares two effect estimates using their standard errors via z-score calculation.
#' This implements the method described in doi: 10.1038/s41380-019-0596-9
#'
#' @param b1 Numeric. First coefficient/effect estimate
#' @param b2 Numeric. Second coefficient/effect estimate
#' @param se1 Numeric. Standard error of first estimate
#' @param se2 Numeric. Standard error of second estimate
#'
#' @return A named list with components:
#'   \item{z_score}{The z-score testing the difference}
#'   \item{p_value}{Two-tailed p-value}
#'   \item{difference}{Difference between coefficients (b1 - b2)}
#'   \item{se_difference}{Standard error of the difference \eqn{\sqrt{se1^2 + se2^2}}}
#'
#' @export
#'
#' @examples
#' # Get z-score
#' res <- compare_coefs(0.5, 0.3, 0.1, 0.12)
#' res$z_statistic
#'
#' # Get p-value
#' res$p_value
compare_coefs <- function(b1, b2, se1, se2) {
  validate_params(
    custom_checks = list(
      list(
        condition = is.numeric(b1) && length(b1) == 1,
        message = "{.arg b1} must be a single numeric value"
      ),
      list(
        condition = is.numeric(b2) && length(b2) == 1,
        message = "{.arg b2} must be a single numeric value"
      ),
      list(
        condition = is.numeric(se1) && length(se1) == 1 && se1 > 0,
        message = "{.arg se1} must be a single positive numeric value"
      ),
      list(
        condition = is.numeric(se2) && length(se2) == 1 && se2 > 0,
        message = "{.arg se2} must be a single positive numeric value"
      )
    ),
    context = "compare_coefs"
  )

  z <- (b1 - b2) / sqrt(se1^2 + se2^2)
  p_val <- 2 * (1 - pnorm(abs(z)))

  all_results <- list(
    z_statistic = z,
    p_value = p_val,
    diff = b1 - b2,
    se_diff = sqrt(se1^2 + se2^2)
  )

  return(all_results)
}
