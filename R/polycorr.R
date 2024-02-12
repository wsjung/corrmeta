#' Calculate Polychoric Correlations
#'
#' This function calculates the polychoric correlations between pairs of variables
#' in a given data frame. It returns a data frame with the row and column names of
#' the variables, the polychoric correlation coefficient, and its standard error.
#'
#' @param data A data frame with variables to correlate
#' @param varlist A vector of variable names to correlate
#' @return A data frame with polychoric correlation coefficients and standard errors
#' @importFrom polycor polychor
#' @export
#' @examples
#'   df <- data.frame(scan1 = runif(10),
#'                    scan2 = runif(10),
#'                    scan3 = runif(10),
#'                    markname = LETTERS[1:10])
#'   varlist <- c("scan1", "scan2", "scan3")
#'   polycorr(df, varlist)
#'
#' @author Woo Jung
#' @keywords correlation, polychoric
#' @seealso \code{\link{polychor}}
polycorr <- function(data, varlist) {

  # Create empty data frame to store correlations
  polycorrs <- data.frame(row = character(), col = character(),
                          PLCORR = numeric(), E_PLCORR = numeric())

  # Store diagonal correlations
  for (i in 1:length(varlist)) {
    vari <- varlist[i]


    # Loop over remaining variables and compute correlation
    for (j in 1:length(varlist)) {
      varj <- varlist[j]

      # Store diagonal correlation
      if (vari == varj) {
        temp_df <- data.frame(row = vari, col = vari,
                              PLCORR = 1, E_PLCORR = 0)
        polycorrs <- rbind(polycorrs, temp_df)
        next
      }

      # Compute correlation
      corr <- polycor::polychor(table(data[, c(vari,varj)]), ML=TRUE)

      # TODO: compute std err

      # Store
      temp_df <- data.frame(row = vari, col = varj,
                            PLCORR = corr, E_PLCORR = 0)
      polycorrs <- rbind(polycorrs, temp_df)
    }
  }

  polycorrs
}
