#' @title Define Class for Multiple Imputation Model with Additional Information
#'
#' @description
#' This function defines an S4 class `imputeMulti` that extends the `mod_imputeMulti` class,
#' adding additional slots to store more detailed information about the multiple imputation model.
#' The class includes slots for the call used to fit the model, the method, the number of iterations,
#' log-likelihood, conjugate prior type, parameter estimates, imputation data, and the number of missing values.
#'
#' @slot Gcall A call object representing the original call used to invoke the imputation model.
#' @slot method A character string specifying the method used for imputation (e.g., "EM", "DA").
#' @slot mle_call A call object for the model fitting process.
#' @slot mle_iter A numeric value indicating the number of iterations performed in the model fitting process.
#' @slot mle_log_lik A numeric value representing the log-likelihood of the fitted model.
#' @slot mle_cp A character string specifying the conjugate prior used in the model.
#' @slot mle_x_y A data frame containing the model's parameter estimates and counts.
#' @slot data A list containing the input data used for imputation, which can include observed and missing values.
#' @slot nmiss A numeric value indicating the number of missing values in the data.
#'
#' @return An S4 object of class `imputeMulti`, which extends the `mod_imputeMulti` class and contains
#'         the information stored in the defined slots.
#'
#' @details
#' The `imputeMulti` class is designed for multiple imputation models with enhanced functionality.
#' It extends the `mod_imputeMulti` class by adding additional slots to store the call used to create the
#' imputation, the imputation method, and details about the data, such as the number of missing values.
#' This allows users to keep track of all relevant model information, including the missing data pattern,
#' during the imputation process.
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#'
#' @export
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
