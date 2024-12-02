#' @title Function that checks if the target object is a \code{mod_imputeMulti} object.
#'
#' @param x
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples

is.imputeMulti <- function(x) {
  inherits(x, "imputeMulti")
}
