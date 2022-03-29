#' nhpp
#'
#' This function simulates a non-homogeneous Poisson process.
#'
#' @param alpha Shape parameter of gamma distribution for total demand. Default 200.
#' @param beta Rate parameter of gamma distribution for total demand. Default 1.
#' @param a1 Beta distribution parameter 1 for customer type 1. Default 1.
#' @param b1 Beta distribution parameter 2 for customer type 1. Default 1.
#' @param a2 Beta distribution parameter 1 for customer type 2. Default 1.
#' @param b2 Beta distribution parameter 2 for customer type 2. Default 1.
#' @param phi1 Percentage of total demand for customer type 1. Default 0.5.
#' @param n Total number of time points that will be generated. Default 3000.
#' @param hor_length Number of points that make up the booking horizon, Default 30.
#' @return A numeric value.
#' @export

nhpp <- function(alpha = 200,
                 beta = 1,
                 phi1 = 0.5,
                 a1 = 1,
                 b1 = 1,
                 a2 = 1,
                 b2 = 1,
                 n = 3000,
                 hor_length = 30) {
  A <- stats::rgamma(1, shape = alpha, rate = beta) / hor_length
  phi2 <- 1 - phi1
  Cust1 <- numeric(length = n)
  Cust2 <- numeric(length = n)
  for (t in seq_len(n)) {
    l1 <- nhpprate(A, a1, b1, phi1, t, n) * (hor_length / n)
    Cust1[t] <- stats::rpois(1, l1)
    l2 <- nhpprate(A, a2, b2, phi2, t, n) * (hor_length / n)
    Cust2[t] <- stats::rpois(1, l2)
  }
  return(list(Cust1 = Cust1, Cust2 = Cust2))
}
