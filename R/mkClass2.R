#' @title Make A Class
#'
#' @slot Gcall call.
#' @slot method character.
#' @slot mle_call call.
#' @slot mle_iter numeric.
#' @slot mle_log_lik numeric.
#' @slot mle_cp character.
#' @slot mle_x_y data.frame.
#' @slot data list.
#' @slot nmiss numeric.
#'
#' @return
#' @export
#'
#' @examples
setClass("imputeMulti",
         representation= list(Gcall= "call",
                              method= "character",
                              mle_call= "call",
                              mle_iter= "numeric",
                              mle_log_lik= "numeric",
                              mle_cp= "character",
                              mle_x_y= "data.frame",
                              data= "list",
                              nmiss= "numeric"),
         contains= "mod_imputeMulti")
