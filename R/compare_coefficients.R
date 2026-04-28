#' Compare two coefficients via a z-score test
#'
#' Tests whether two effect estimates differ, given their standard errors,
#' under the assumption of independent estimates. Implements the z-score
#' approach described in \doi{10.1038/s41380-019-0596-9}.
#'
#' @param b1,b2 Numeric scalars: the two coefficient estimates.
#' @param se1,se2 Numeric scalars: standard errors of the two estimates.
#'   Must be positive.
#'
#' @return A named list with:
#'   * `z_statistic` --- the z-score for the difference.
#'   * `p_value` --- two-tailed p-value.
#'   * `diff` --- `b1 - b2`.
#'   * `se_diff` --- \eqn{\sqrt{se1^2 + se2^2}}.
#'
#' @examples
#' compare_coefs(b1 = 0.50, b2 = 0.30, se1 = 0.10, se2 = 0.12)
#'
#' @export
compare_coefs <- function(b1, b2, se1, se2) {
  validate_args(
    b1  = is_number(),
    b2  = is_number(),
    se1 = is_number(positive = TRUE),
    se2 = is_number(positive = TRUE)
  )

  se_diff <- sqrt(se1^2 + se2^2)
  z       <- (b1 - b2) / se_diff
  # Use pnorm(-|z|) rather than 1 - pnorm(|z|) to avoid catastrophic
  # cancellation when |z| is large (which would otherwise underflow to 0).
  p       <- 2 * stats::pnorm(-abs(z))

  list(
    z_statistic = z,
    p_value     = p,
    diff        = b1 - b2,
    se_diff     = se_diff
  )
}
