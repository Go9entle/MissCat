#' @title Get Level Text
#'
#' @param var Name of the categorical variable.
#' @param val The positive integers converted to categorical variable factor levels.
#'
#' @return Text form of factor levels for categorical variables.
#' @export
#'
get_level_text <- function(var, val) {
  lvls <- levels(var)
  return(lvls[val])
}
