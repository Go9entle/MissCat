#' @title Check if the target object is a \code{mod_imputeMulti} object
#'
#' @description This function checks whether the provided object is of class \code{mod_imputeMulti}.
#'
#' @param x The object to be checked.
#'
#' @return A logical value: \code{TRUE} if \code{x} is a \code{mod_imputeMulti} object, \code{FALSE} otherwise.
#' @export
#'
is.mod_imputeMulti <- function(x) {
  inherits(x, "mod_imputeMulti")
}

