#' Convert P-values to Z-scores
#'
#' This function takes a data frame of p-values and converts them to Z-scores
#' using the quantile function for the standard normal distribution.
#'
#' @param df_pvalues data frame containing p-values
#' @return data frame containing Z-scores
#' @export
#' @examples
#'   data(snp_example)
#'   head(snp_example)
#'   pvalues_to_zscores(snp_example)
#'
#' @importFrom dplyr mutate_at vars
#' @importFrom magrittr %>%
#' @importFrom stats qnorm
#' @export
#'
#' @author Woo Jung
#' @seealso \code{\link{qnorm}}
#'
pvalues_to_zscores <- function(df_pvalues) {

  df_zscores <- df_pvalues %>%
    dplyr::mutate_at(dplyr::vars(-markname), ~qnorm(.x, lower.tail = FALSE))

  df_zscores
}
#' Generates a list of random p-values with mixed significant and insignificant values
#'
#' @param n number of samples
#' @param ratio_significant fraction of p-values to be significant
#' @return list of n randomly generated significant and insignificant p-values
#' @importFrom stats runif
#' @export
#'
generate_random_p_values <- function(n, ratio_significant) {

  p_values <- runif(n, 0.05, 1)
  num_sig <- round(n * ratio_significant)
  sig_idx <- sample(1:n, num_sig)
  p_values[sig_idx] <- runif(num_sig, 0, 0.05)

  p_values
}
