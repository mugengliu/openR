#' Calculate Odds Ratio and its 95% Confidence Interval
#'
#' This function calculates the Odds Ratio (OR) and its 95% Confidence Interval (CI)
#' given the coefficient, standard error, significance level, and rounding precision.
#'
#' @param coef Coefficient from a logistic regression model (numeric).
#' @param se Standard error of the coefficient (numeric).
#' @param siglevel Significance level (numeric; e.g., 0.05 for 95% CI).
#' @param roundto Number of decimal places to round to (integer).
#'
#' @return A string representing the Odds Ratio and its Confidence Interval formatted as "OR (lower CI, upper CI)".
#'
#' @examples
#' # Example values
#' coef <- 0.5          # Coefficient from logistic regression
#' se <- 0.1            # Standard error of the coefficient
#' siglevel <- 0.05     # Significance level for 95% CI
#' roundto <- 2         # Round to 2 decimal places
#'
#' # Calculate Odds Ratio and 95% CI
#' OR_result <- OR_95CI(coef, se, siglevel, roundto)
#' print(OR_result)  # Expected Output: "1.65 (1.23, 2.21)"
#'
#' # Another example
#' coef <- -0.3        # Another coefficient
#' se <- 0.15          # Corresponding standard error
#' OR_result2 <- OR_95CI(coef, se, siglevel, roundto)
#' print(OR_result2)  # Expected Output: "0.74 (0.54, 1.01)"
#'
#' @export
OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
