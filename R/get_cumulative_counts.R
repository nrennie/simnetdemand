#' Get cumulative counts
#'
#' This function applies cumsum to each row of a dataframe or matrix
#'
#' @param data A numeric matrix or dataframe
#' @return A matrix or dataframe
#' @export

get_cumulative_counts <- function(data) {
  df <- t(apply(data, 1, cumsum))
  return(df)
}
