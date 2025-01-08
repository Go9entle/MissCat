#' @title Define Class for Multiple Imputation Model
#'
#' @description
#' This function defines an S4 class `mod_imputeMulti` that is used to store
#' information about a multiple imputation model. The class includes slots for
#' storing the method used (such as "EM" or "DA"), the call used to invoke the
#' model fitting, the number of iterations, the log-likelihood, and the model parameters.
#'
#' @slot method A character string indicating the method used for imputation.
#'              Possible values include "EM" (Expectation-Maximization) and "DA" (Dirichlet Allocation).
#' @slot mle_call The call used to invoke the model fitting.
#' @slot mle_iter A numeric value specifying the number of iterations completed in the fitting process.
#' @slot mle_log_lik A numeric value representing the log-likelihood of the fitted model.
#' @slot mle_cp A character string indicating the type of conjugate prior used in the model.
#' @slot mle_x_y A data frame containing the model's parameter estimates and counts.
#'
#' @return An S4 object of class `mod_imputeMulti` containing the information described in the slots.
#'
#' @details
#' The `mod_imputeMulti` class is designed to store the results of fitting
#' a multiple imputation model using methods such as Expectation-Maximization (EM) or
#' Dirichlet Allocation (DA). The class validates the method and the number of iterations
#' to ensure that they are appropriate for the model fitting process. The `validity` function
#' checks that the `method` is either "EM", "DA", or "NULL" and that the number of iterations
#' is non-negative.
#'
#' @export
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
