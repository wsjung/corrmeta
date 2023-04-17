#' Convert P-values to Z-scores
#'
#' This function takes a data frame of p-values and converts them to Z-scores
#' using the quantile function for the standard normal distribution.
#'
#' @param df_pvalues A data frame containing p-values
#' @return A data frame containing Z-scores
#' @examples
#'   df <- data.frame(p1 = c(0.05, 0.01, 0.001), p2 = c(0.1, 0.2, 0.3),
#'                    markname = c("A", "B", "C"))
#'   pvalues_to_zscores(df)
#'
#' @import dplyr
#' @importFrom stats qnorm
#'
#' @author Woo Jung
#' @keywords p-values, Z-scores
#' @seealso \code{\link{qnorm}}
#'
#' @examples
#'   df <- data.frame(p1 = c(0.05, 0.01, 0.001), p2 = c(0.1, 0.2, 0.3),
#'                    markname = c("A", "B", "C"))
#'   pvalues_to_zscores(df)
pvalues_to_zscores <- function(df_pvalues) {

  df_zscores <- df_pvalues %>%
    mutate_at(vars(-markname), qnorm)

  df_zscores
}
