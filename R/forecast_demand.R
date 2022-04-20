#' Forecast demand
#'
#' This function forecasts demand per fare class
#'
#' @param cust Numeric vector of customer arrivals per time period
#' @param probs Numeric vector of probability of purchase in each fare class
#' @return A numeric value.
#' @export

forecast_demand <- function(cust, probs) {
  n <- length(cust)
  p <- length(probs) - 1
  demand <- list(A = numeric(n),
                 O = numeric(n),
                 J = numeric(n),
                 P = numeric(n),
                 R = numeric(n),
                 S = numeric(n),
                 M = numeric(n))
  for (i in seq_len(n)) {
    if (cust[i] != 0) {
      k <- sample(0:p, cust[i], prob = probs, replace = TRUE)
      if (length(k) != 0) {
        for (j in seq_len(k)) {
          if (k[j] != 0) {
            demand[[k[j]]][i] <- cust[i]
          }
        }
      }
    }
  }
  return(demand)
}
