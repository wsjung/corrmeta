#' Calculate Tetrachoric Correlations
#'
#' This function calculates the tetrachoric correlations between pairs of variables
#' in a given data frame. It returns a list containing a data frame with the tetrachoric
#' correlation coefficients, and the sum of the tetrachoric correlations if the
#' input variable was in p-value form.
#'
#' @param data data frame with "markname" and study names as column names.
#' @param varlist character vector of study names to include in the meta-analysis.
#' @return list containing a data frame with tetrachoric correlation coefficients,
#'         and the sum of the tetrachoric correlations if the input variable was in
#'         p-value form.
#' @importFrom polycor polychor
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @export
#'
#' @author Woo Jung
#' @seealso \code{\link[polycor]{polychor}}
#'
#' @examples
#'   data(snp_example)
#'   head(snp_example)
#'   varlist <- c("trt1","trt2","trt3")
#'   tetracorr(snp_example, varlist)
tetracorr <- function(data, varlist) {

  # apply probit transformation to p-values
  data <- pvalues_to_zscores(data)

  # binarize probit transformed p-values
  data[, -1] <- lapply(data[, -1], function(x) as.integer(x > 0))

  # calculate tetrachoric correlation
  df_sigma <- polycorr(data, varlist)

  # calculate cumulative sum of p-values (sumsigmadsn)
  sum_sigma <- sum(df_sigma$PLCORR)


  # reshape to study x study
  # TODO: reshape2 is deprecated
  df_sigma <- df_sigma %>%
    tidyr::pivot_wider(id_cols=row, names_from=col, values_from=PLCORR)

  list("sigma" = df_sigma, "sum_sigma" = sum_sigma)
}
if(getRversion() >= "4.3.2")  utils::globalVariables(c("PLCORR"))
