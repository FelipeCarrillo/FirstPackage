
#' @title Deaggregate rows
#'
#' @param data A dataset with multiple rows
#' @param var The variable that the user want to split in multiple rows
#'
#' @return An aggregated dataset larger than the original
#' @export
#'
#' @examples
#' a <- data.frame(name= c("Maria", "Tina"), age= c(5,3))
#' a
#' one2many(a,'age')
one2many <- function(data, var){
data$rows <- seq(nrow(data))
data <- data[rep(1:nrow(data),data[,var]),]
return(data)
}
