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
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @author Woo Jung
#' @keywords correlation, tetrachoric
#' @seealso \code{\link{polychor}}
#'
#' @examples
#'   data(mtcars)
#'   vars <- c("mpg", "cyl", "hp", "drat", "wt", "qsec")
#'   tetracorr(mtcars, vars)
tetracorr <- function(data, vars) {

  # apply probit transformation to p-values
  data <- pvalues_to_zscores(data)

  # binarize probit transformed p-values
  data[, -1] <- lapply(data[, -1], function(x) as.integer(x > 0))

  # calculate tetrachoric correlation
  df_sigma <- polycorr(data, vars)

  # calculate cumulative sum of p-values (sumsigmadsn)
  sum_sigma <- sum(df_sigma$PLCORR)


  # reshape to study x study
  # TODO: reshape2 is deprecated
  df_sigma <- df_sigma %>%
    tidyr::pivot_wider(id_cols=row, names_from=col, values_from=PLCORR)

  list("sigma" = df_sigma, "sum_sigma" = sum_sigma)
}
