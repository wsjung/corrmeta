#' Convert P-values to Z-scores
#'
#' This function takes a data frame of p-values and converts them to Z-scores
#' using the quantile function for the standard normal distribution.
#'
#' @param df_pvalues A data frame containing p-values
#' @return A data frame containing Z-scores
#' @export
#' @examples
#'   df <- data.frame(scan1 = runif(10),
#'                    scan2 = runif(10),
#'                    scan3 = runif(10),
#'                    markname = LETTERS[1:10])
#'   pvalues_to_zscores(df)
#'
#' @importFrom dplyr mutate_at
#' @importFrom stats qnorm
#'
#' @author Woo Jung
#' @keywords p-values, Z-scores
#' @seealso \code{\link{qnorm}}
#'
pvalues_to_zscores <- function(df_pvalues) {

  df_zscores <- df_pvalues %>%
    dplyr::mutate_at(vars(-markname), ~qnorm(.x, lower.tail = FALSE))

  df_zscores
}

generate_random_p_values <- function(n, ratio_significant) {

  p_values <- runif(n, 0.05, 1)
  num_sig <- round(n * ratio_significant)
  sig_idx <- sample(1:n, num_sig)
  p_values[sig_idx] <- runif(num_sig, 0, 0.05)

  p_values
}
