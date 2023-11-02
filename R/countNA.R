#' @title Count NAs in dataset

#' @description Counts NAs

#' @param  dataframe NAs by column
#' @param dataframe NAs by column

#' @return dataframe adding NAs by column

#' @examples

#' dataset <- matrix(sample(c(NA, 1:5),25, replace = TRUE), 5)

#' df <- as.data.frame(dataset)

#'
#' @export


countNA <- function(dataframe) {
colSums(is.na(dataframe))
}
