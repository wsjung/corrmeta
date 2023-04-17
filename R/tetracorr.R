#' Calculate Tetrachoric Correlations
#'
#' This function calculates the tetrachoric correlations between pairs of variables
#' in a given data frame. It returns a list containing a data frame with the tetrachoric
#' correlation coefficients, and the sum of the tetrachoric correlations if the
#' input variable was in p-value form.
#'
#' @param data A data frame with variables to correlate
#' @param vars A vector of variable names to correlate
#' @return A list containing a data frame with tetrachoric correlation coefficients,
#'         and the sum of the tetrachoric correlations if the input variable was in
#'         p-value form.
#' @importFrom polycor polychor
#' @export
#' @examples
#'   data(mtcars)
#'   vars <- c("mpg", "cyl", "hp", "drat", "wt", "qsec")
#'   tetracorrs(mtcars, vars)
#'
#' @author Woo Jung
#' @keywords correlation, tetrachoric
#' @seealso \code{\link{polychor}}
#'
#' @examples
#'   data(mtcars)
#'   vars <- c("mpg", "cyl", "hp", "drat", "wt", "qsec")
#'   tetracorrs(mtcars, vars)
tetracorr <- function(data, vars) {

  # apply probit transformation to p-values
  data <- pvalues_to_zscores(data)

  # binarize probit transformed p-values
  data[, -1] <- lapply(data[, -1], function(x) as.integer(x > 0))

  # calculate tetrachoric correlation
  df_sigma <- polycorr(data, vars)

  # calculate cumulative sum of p-values (sumsigmadsn)
  if (coefft == 1) {
    sum_sigma <- sum(df_sigma$PLCORR)
  }

  # reshape to study x study
  df_sigma <- dcast(df_sigma, row ~ col, value.var = "PLCORR")

  list("sigma" = df_sigma, "sum_sigma" = sum_sigma)
}
