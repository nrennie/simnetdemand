#' nhpprate
#'
#' This function determines the rate of a non-homogeneous Poisson process.
#'
#' @param A Mean total demand across all customer types. Default 200.
#' @param a Beta distribution parameter 1. Default 1.
#' @param b Beta distribution parameter 2. Default 1.
#' @param phi Percentage of total demand for this customer type. Default 1.
#' @param t Current time point. Default 1.
#' @param n Total number of time points that will be generated. Default 3000.
#' @return A numeric value.
#' @export

nhpprate <- function(A = 240,
                     a = 1,
                     b = 1,
                     phi = 1,
                     t = 1,
                     n = 3000) {
  l <- A * phi * stats::dbeta(t / n, a, b)
  return(l)
}
