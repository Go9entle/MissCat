#' @title Convert the factor levels of categorical variables to positive integers.
#'
#' @param f A column of factor variables
#'
#' @return Factor levels converted to a column of positive integers.
#' @export
#'
#' @examples
fact_to_int <- function(f) {
  if (is.factor(f)) {
    l <- levels(f) #l是因子水平的向量
    return(unlist(
      sapply(f, function(i) {
        ifelse(!is.na(i), which(i == l), NA)
        #如果不是NA,则找到i在因子水平向量中的位置(整数编码)

      })
    ))
  } else {
    return(f)
  }
}
