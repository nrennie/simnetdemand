#' Forecast demand by simulation
#'
#' This function forecasts demand per fare class
#'
#' @param nsim Number of simulations Default 100.
#' @param probs1 Numeric vector of probability of purchase in each fare class
#' for customer type 1
#' @param probs2 Numeric vector of probability of purchase in each fare class
#' for customer type 2
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

forecast_demand_sim <- function(nsim = 100,
                                probs1,
                                probs2,
                                alpha = 200,
                                beta = 1,
                                a1 = 1,
                                b1 = 1,
                                a2 = 1,
                                b2 = 1,
                                phi1 = 0.5,
                                n = 3000,
                                hor_length = 30) {
  cust <- nhpp(alpha = alpha,
               beta = beta,
               a1 = a1,
               b1 = b1,
               a2 = a2,
               b2 = b2,
               phi1 = phi1,
               n = n,
               hor_length = hor_length)
  cust1 <- cust[[1]]
  cust2 <- cust[[2]]
  Ademand <- numeric(length = nsim)
  Odemand <- numeric(length = nsim)
  Jdemand <- numeric(length = nsim)
  Pdemand <- numeric(length = nsim)
  Rdemand <- numeric(length = nsim)
  Sdemand <- numeric(length = nsim)
  Mdemand <- numeric(length = nsim)
  for (i in seq_len(nsim)) {
    D1 <- forecast_demand(cust = cust1, probs = probs1)
    D2 <- forecast_demand(cust = cust2, probs = probs2)
    Demand <- list(A = D1$A + D2$A,
                   O = D1$O + D2$O,
                   J = D1$J + D2$J,
                   P = D1$P + D2$P,
                   R = D1$R + D2$R,
                   S = D1$S + D2$S,
                   M = D1$M + D2$M)
    Ademand[i] <- sum(Demand$A)
    Odemand[i] <- sum(Demand$O)
    Jdemand[i] <- sum(Demand$J)
    Pdemand[i] <- sum(Demand$P)
    Rdemand[i] <- sum(Demand$R)
    Sdemand[i] <- sum(Demand$S)
    Mdemand[i] <- sum(Demand$M)
  }
  out <- list(means = c(mean(Ademand),
                mean(Odemand),
                mean(Jdemand),
                mean(Pdemand),
                mean(Rdemand),
                mean(Sdemand),
                mean(Mdemand)),
              vars = c(stats::var(Ademand),
                stats::var(Odemand),
                stats::var(Jdemand),
                stats::var(Pdemand),
                stats::var(Rdemand),
                stats::var(Sdemand),
                stats::var(Mdemand)))
  return(out)
}
