#' @title Expectation-Maximization for Multinomial Data with Missing Values
#'
#' @description This function implements the Expectation-Maximization (EM) algorithm to
#' estimate the parameters for multinomial data with missing values. It performs
#' both the E-step (Expectation) and M-step (Maximization) iteratively until convergence.
#' @import data.table
#' @import gtools
#' @import stats
#' @import methods
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#' @param dat A data frame containing the data with missing values. All columns
#'            must be factors. Missing values are handled by the algorithm.
#' @param conj_prior A character string specifying the type of prior to use.
#'                   Options include:
#'        - "none": No prior distribution is used.
#'        - "data.dep": Data-dependent prior.
#'        - "flat.prior": A flat prior is used.
#'        - "non.informative": A non-informative Jeffrey's prior.
#' @param alpha A numeric vector specifying the prior parameters. Required if
#'              `conj_prior` is "data.dep" or "flat.prior". It represents the
#'              parameters of the prior distribution.
#' @param tol A numeric value indicating the convergence tolerance. Default is
#'            5e-7. The algorithm terminates when the change in parameters is
#'            smaller than this threshold.
#' @param max_iter An integer specifying the maximum number of iterations. Default
#'                 is 10000. The algorithm will stop if this number of iterations
#'                 is reached without converging.
#'
#' @return An object of class `mod_imputeMulti` containing:
#' \item{method}{The method used for imputation, which is always "EM".}
#' \item{mle_call}{The call to the function that generated the model.}
#' \item{mle_iter}{The number of iterations performed during the EM algorithm.}
#' \item{mle_log_lik}{The log-likelihood of the fitted model.}
#' \item{mle_cp}{The conjugate prior used in the model.}
#' \item{mle_x_y}{A data frame containing the estimated parameters and counts,
#'                including the updated parameter estimates and sufficient statistics.}
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data("tract2221")
#' dat <- tract2221[,c(1:2,4,7,9)]
#' result <- EM_multinomial(dat, conj_prior = "non.informative")
EM_multinomial <- function(dat,
                           conj_prior = c("none", "data.dep", "flat.prior", "non.informative"),
                           alpha = NULL, tol = 5e-7, max_iter = 10000) {

  # 0. Check for errors and handle invalid inputs
  conj_prior <- match.arg(conj_prior, several.ok = FALSE) # Validate prior options
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha)) {
    stop("Please supply argument alpha as prior.") # Error if alpha is required but not provided
  }

  mc <- match.call() # Capture the call to this function
  p <- ncol(dat) # Number of columns (variables) in the data
  n_obs <- nrow(dat) # Number of observations in the data

  # 1. Data processing: Convert data to appropriate format and handle missing values
  data.table::setDT(dat) # Convert to data.table for efficiency
  if (!all(unlist(lapply(dat, is.factor)))) {dat <- dat[, lapply(.SD, as.factor)]}
  # Ensure all columns are factors, otherwise convert them

  # Create all possible combinations of factor levels for complete and missing data
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
  enum_comp <- enum[stats::complete.cases(enum),] # Complete cases (non-missing)
  enum_miss <- enum[!stats::complete.cases(enum),] # Missing cases (with missing values)
  enum_miss <- enum_miss[apply(enum_miss, 1, function(x) !all(is.na(x))),] # Exclude rows where all values are NA

  data.table::setDT(enum_comp) # Convert to data.table for efficient indexing
  rownames(enum_comp) <- seq_len(nrow(enum_comp)) # Set row names for easy referencing

  # Split the dataset into complete and incomplete data
  dat_comp <- dat[stats::complete.cases(dat),] # Complete data
  dat_miss <- dat[!stats::complete.cases(dat),] # Incomplete data (missing values)

  # Convert data to integer representation for efficient computation
  enum_comp_int <- do.call("cbind", lapply(enum_comp, fact_to_int))
  enum_miss_int <- do.call("cbind", lapply(enum_miss, fact_to_int))
  dat_miss_int <- do.call("cbind", lapply(dat_miss, fact_to_int))

  # Perform efficient merging and matching using auxiliary functions
  enum_comp_int1 <- as.data.table(enum_comp_int)
  z_Os_y0 <- data.table::setDT(merge_duplicate_rows_large(dat_miss_int))
  z_Os_y <- data.table::setDT(match_rows_and_add_rowname(z_Os_y0, enum_miss_int))
  z_Os_y1 <- z_Os_y[, rowname := as.integer(rownames(z_Os_y))]
  setorder(z_Os_y1, rowname)
  rownames(z_Os_y1) <- z_Os_y1$rowname
  z_Os_y <- z_Os_y1[, rowname := NULL] # Remove the temporary rowname column

  search_out <- mx_my_compare(z_Os_y[, 1:p], enum_comp_int1) # Efficient matching of incomplete data
  rm(z_Os_y0, z_Os_y1) # Clean up intermediate variables

  # Match complete data to all possible complete patterns
  dat_comp_int <- do.call("cbind", lapply(dat_comp, fact_to_int))
  x_y0 <- data.table::setDT(merge_duplicate_rows_large(dat_comp_int))
  x_y <- data.table::setDT(match_rows_and_add_rowname(x_y0, enum_comp_int))

  x_y[, rowname := as.integer(rownames(x_y))]
  setorder(x_y, rowname)
  rownames(x_y) <- x_y$rowname
  x_y[, rowname := NULL]

  # Apply prior distribution to the complete patterns
  enum_comp <- check_prior(conj_prior = conj_prior, alpha = alpha, outer = FALSE, enum_comp = enum_comp)
  comp_ind <- search_out

  # 2. E-step and M-step of the EM algorithm
  iter <- 0
  while (iter < max_iter) {
    # E-Step: Calculate the expected counts and log-likelihood
    log_lik <- log_lik0 <- 0
    enum_comp$counts <- 0

    for (s in 1:nrow(z_Os_y)) {
      b_Os_y <- sum(enum_comp$theta_y[unlist(comp_ind[[s]])])
      E_Xsy_Zy_theta <- z_Os_y$counts[s] * enum_comp$theta_y[unlist(comp_ind[[s]])] / b_Os_y
      enum_comp$counts[unlist(comp_ind[[s]])] <- enum_comp$counts[unlist(comp_ind[[s]])] + E_Xsy_Zy_theta

      if (b_Os_y > 0) {
        log_lik <- log_lik + z_Os_y$counts[s] * log(b_Os_y)
      }
    }

    # Add observed counts to the expected counts
    enum_comp$counts[as.integer(rownames(x_y))] <- enum_comp$counts[as.integer(rownames(x_y))] + x_y$counts
    log_lik <- log_lik + sum(ifelse(enum_comp$theta_y[as.integer(rownames(x_y))] == 0, 0,
                                    x_y$counts * log(enum_comp$theta_y[as.integer(rownames(x_y))])))

    # M-Step: Maximize the log-likelihood and update the parameters
    if (conj_prior == "none") {
      enum_comp$theta_y1 <- enum_comp$counts / n_obs
    } else {
      D <- nrow(enum_comp)
      alpha_0 <- sum(enum_comp$alpha)
      enum_comp$theta_y1 <- (enum_comp$counts + enum_comp$alpha - 1) / (n_obs + alpha_0 - D)
    }

    # Check for convergence
    iter <- iter + 1
    if (supDist(enum_comp$theta_y, enum_comp$theta_y1) < tol | abs(log_lik - log_lik0) < tol * 100) {
      # Update log-likelihood for prior
      if (conj_prior != "none") {
        log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                        enum_comp$alpha * log(enum_comp$theta_y)))
      }
      enum_comp$theta_y1 <- NULL # Remove temporary variable
      enum_comp$counts <- NULL # Remove counts

      # Return the fitted model as an object of class 'mod_imputeMulti'
      mod <- methods::new("mod_imputeMulti",
                          method = "EM",
                          mle_call = mc,
                          mle_iter = iter,
                          mle_log_lik = log_lik,
                          mle_cp = conj_prior,
                          mle_x_y = enum_comp)
      return(mod)

    } else {
      enum_comp$theta_y <- enum_comp$theta_y1 # Update parameters for the next iteration
      log_lik0 <- log_lik
    }
  }

  # If max_iter is exceeded, return the model
  if (conj_prior != "none") {
    log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                    enum_comp$alpha * log(enum_comp$theta_y)))
  }
  enum_comp$theta_y1 <- NULL
  enum_comp$counts <- NULL

  mod <- methods::new("mod_imputeMulti",
                      method = "EM",
                      mle_call = mc,
                      mle_iter = iter,
                      mle_log_lik = log_lik,
                      mle_cp = conj_prior,
                      mle_x_y = enum_comp)
  return(mod)
}
