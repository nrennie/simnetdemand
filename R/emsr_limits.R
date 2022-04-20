

#' EMSR Limits
#'
#' This function calculates the EMSR booking limits
#'
#' @param fares Vector of fares.
#' @param means Vector of demand mean forecast for each fare class.
#' @param vars Vector of demand variance forecast for each fare class.
#' @param capacity nteger for total capacity.
#' @return A numeric vector
#' @export

emsr_limits <- function(fares,
                        means,
                        vars,
                        capacity) {
  n <- length(fares)
  prolimits <- numeric(n)
  for (j in seq_len(n - 1)) {
    m <- sum(means[1:j])
    sd <- sqrt(sum(vars[1:j]))
    if ((fares[j + 1] / (sum(fares[1:j] * means[1:j]) / m)) >= 1) {
      if (j == 1) {
        prolimits[j] <- 0
      }
      else {
        prolimits[j] <- prolimits[j - 1]
      }
    }
    else {
      prolimits[j] <- round(min(max(0,
                                    stats::qnorm(
                                      (1 - (fares[j + 1] / (sum(fares[1:j] * means[1:j]) / m))),
                                       mean = m,
                                       sd = sd)),
                                capacity))
    }
  }
  prolimits[n] <- capacity
  return(c(prolimits[1], diff(prolimits)))
}
