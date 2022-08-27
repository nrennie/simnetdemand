#' Aggregate booking horizon
#'
#' This function aggregates multiple time points into a booking horizon
#'
#' @param data Numeric vector of demand
#' @param bh_len Length of booking horizon. Default 30.
#' @return A numeric vector
#' @export

agg_bh <- function(data,
                   bh_len = 30) {
  k <- length(data) / bh_len
  if (k %% 1 != 0) {
    return("vector not an integer multiple of booking horizon")
  } else {
    output <- unname(tapply(data, (seq_along(data) - 1) %/% k, sum))
    return(output)
  }
}
