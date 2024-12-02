#' @title Making a Class
#'
#' @slot method character.
#' @slot mle_call call.
#' @slot mle_iter numeric.
#' @slot mle_log_lik numeric.
#' @slot mle_cp character.
#' @slot mle_x_y data.frame.
#'
#' @return
#' @export
#'
#' @examples
setClass("mod_imputeMulti",
         representation= list(
           method= "character",
           mle_call= "call",
           mle_iter= "numeric",
           mle_log_lik= "numeric",
           mle_cp= "character",
           mle_x_y= "data.frame"),
         validity= function(object) {
           if (!object@method %in% c("EM", "DA", "NULL")) {
             return("Currently only EM and DA methods are defined.")
           } else if (object@mle_iter < 0) {
             return("A negative iteration was given.")
           }
           return(TRUE)
         }
)
