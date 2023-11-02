#' @title  Sum total NAs in dataset
#'
#' @param dataframe A dataset with NAs
#'
#' @param data A dataset with NAs
#' @return dataframe with total NAs
#' @export
#'
#' @examples
#'dataset <- matrix(sample(c(NA, 1:5),25, replace = TRUE), 5)
#' df <- as.data.frame(dataset)
#' df

countNA_total <- function(dataframe) {
  sum(colSums(is.na(dataframe)))
}
