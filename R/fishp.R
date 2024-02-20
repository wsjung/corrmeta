#' Calculate Fisher's method p-value and meta-analysis statistics
#'
#' @param df data frame with "markname" and study names as column names.
#' @param vars character vector of study names to include in the meta-analysis.
#' @param df_sigma data frame of tetrachoric correlations.
#' @param sum_sigma sum of tetrachoric correlations.
#'
#' @return A data frame with columns 'markname', 'sum_chisq', 'sum_z',
#' 'sum_sigma_var', 'pvalue', 'meta_z', 'meta_p', 'meta_nlog10p'
#'
#' @importFrom stats pchisq
#' @importFrom stats pnorm
#' @importFrom dplyr mutate_at vars arrange select
#' @importFrom magrittr %>%
#'
#' @export
#' @examples
#'   data(snp_example)
#'   head(snp_example)
#'   varlist <- c("trt1","trt2","trt3")
#'   tc <- tetracorr(snp_example, varlist)
#'   fishp(snp_example, varlist, tc$sigma, tc$sum_sigma)
fishp <- function(df, vars, df_sigma, sum_sigma) {

  # convert p-values to chisq = -2*log(p)
  fisher_chisq <- df %>%
    dplyr::mutate_at(dplyr::vars(-markname), ~ -2 * log(.))

  # convert p-values to z-scores
  fisher_z <- pvalues_to_zscores(df)

  # calculate correlations of z-transformed p-values using genome scan results
  fisher_chisq$sum_chisq <- rowSums(dplyr::select(fisher_chisq, !markname), na.rm=TRUE)
  fisher_z$sum_z <- rowSums(dplyr::select(fisher_z, !markname), na.rm=TRUE)

  # sort datasets
  fisher_chisq <- fisher_chisq %>% dplyr::arrange(markname)
  fisher_z <- fisher_z %>% dplyr::arrange(markname)

  # merge chisq and zscore columns
  fisher <- cbind(dplyr::select(fisher_chisq, markname, sum_chisq), dplyr::select(fisher_z, sum_z))

  # find number of observations
  df$num_obs <- apply(dplyr::select(df, !markname), 1, function(x) sum(!is.na(x)))

  # 1) no missing observations --> sum_sigma_var = sum_sigma
  df[df$num_obs == length(vars), "sum_sigma_var"] <- sum_sigma

  # 2) one observation --> sum_sigma_var = 1
  df[df$num_obs == 1, "sum_sigma_var"] <- 1

  # 3) other (two observations)
  df_other <- df[df$num_obs != length(vars) & df$num_obs != 1, ]
  df_other_sums <- apply(df_other, 1, function(x) {
    row_name <- col_name <- vars[vars %in% names(x)[which(!is.na(x))]]
    col_index <- match(col_name, names(df_sigma))
    row_index <- match(row_name, df_sigma$row)
    sum(df_sigma[row_index, col_index])
  })

  df[df$num_obs < length(vars) & df$num_obs > 1, "sum_sigma_var"] <- df_other_sums
  df <- df %>% dplyr::arrange(markname) # sort

  # concatenate columns
  fisher <- cbind(
    df,
    dplyr::select(fisher, sum_chisq, sum_z)
  )

  # calculate p-value --> SDF of Chi-sq distribution
  fisher$pvalue <- pchisq(fisher$sum_chisq, df=2*length(vars), lower.tail=FALSE)

  # adjust meta_z
  fisher$meta_z <- fisher$sum_z / sqrt(fisher$sum_sigma_var)

  # calculate meta p-value --> SDF of normal distribution
  fisher$meta_p <- pnorm(fisher$meta_z, lower.tail=F)

  # calculate -log10p
  fisher$meta_nlog10p <- -1 * log10(fisher$meta_p)

  fisher
}
if(getRversion() >= "4.3.2")  utils::globalVariables(c("markname","sum_chisq","sum_z"))
