#' @title Check if the target object is a \code{imputeMulti} object
#'
#' @description This function checks whether the provided object is of class \code{imputeMulti}.
#'
#' @param x The object to be checked.
#'
#' @return A logical value: \code{TRUE} if \code{x} is an \code{imputeMulti} object, \code{FALSE} otherwise.
#' @export
#'
is.imputeMulti <- function(x) {
  inherits(x, "imputeMulti")
}
