
#' @title Deaggregate rows with rownames as integers instead of decimals
#'
#' @param data A dataset with multiple rows
#' @param var The variable that the user want to split in multiple rows
#' @return An aggregated dataset larger than the original
#' @import tidyverse tidyr magrittr ggplot2 dplyr
#' @export
#' @name one2many2
#' @examples
#' p_load(tidyverse, Firstpackage)
#' a <- data.frame(name= c("Maria", "Tina"), age= c(5,3))
#' a
#' one2many2(a, age)

library(tidyr)
library(tidyverse)
library(dplyr)
one2many2 <- function(data, var,...){
data %>% mutate(rows = row_number()) |>
uncount({{var}},...)
}
