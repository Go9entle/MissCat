#' @title Impute Missing Values for Multinomial Data
#'
#' This function imputes missing values in categorical data using either
#' Expectation-Maximization (EM) or Dirichlet Allocation (DA) methods.
#' It utilizes prior distributions for the imputation process.
#'
#' @param dat A data.table or data frame containing the data with missing values.
#'            All columns should be factors. If any column is not a factor,
#'            it will be converted to a factor.
#' @param method A character string specifying the imputation method.
#'               Options include:
#'        - "EM": Expectation-Maximization method.
#'        - "DA": Dirichlet Allocation method.
#' @param conj_prior A character string specifying the type of prior to use.
#'                   Options include:
#'        - "none": No prior distribution.
#'        - "data.dep": Data-dependent prior.
#'        - "flat.prior": A flat prior.
#'        - "non.informative": A non-informative Jeffrey's prior.
#' @param alpha A numeric vector specifying the prior parameters.
#'              Required if `conj_prior` is "flat.prior".
#'              If not specified for other priors, the function will handle it accordingly.
#'
#' @return An object of class `imputeMulti` containing:
#' \item{Gcall}{The call to the function, stored as a language object.}
#' \item{method}{The method used for imputation, either "EM" or "DA".}
#' \item{mle_call}{The call used for maximum likelihood estimation.}
#' \item{mle_iter}{The number of iterations performed during the MLE process.}
#' \item{mle_log_lik}{The log-likelihood of the fitted model.}
#' \item{mle_cp}{The conjugate prior used in the imputation process.}
#' \item{mle_x_y}{The estimated parameters for the multinomial distribution,
#'                which are used for imputation.}
#' \item{data}{A list containing two elements:
#'   - `missing_data`: A data.table with the missing values.
#'   - `imputed_data`: A data.table with the missing values imputed.}
#' \item{nmiss}{The number of missing observations in the dataset.}
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#'
#' @examples
#' # Example usage
#' #' data("tract2221")
#' dat <- tract2221[,c(1:2,4,7,9)]
#' imputed_result <- impute4mmv(dat, method = "EM", conj_prior = "non.informative")
#'
#' imputed_result <- impute4mmv(dat, method = "DA", conj_prior = "non.informative")
#' @export
impute4mmv <- function(dat, method = c("EM", "DA"),
                       conj_prior = c("none", "data.dep", "flat.prior", "non.informative"),
                       alpha = NULL){
  set.seed(2024) # Set random seed for reproducibility
  data.table::setDT(dat) # Convert data to data.table format if not already

  # Ensure all columns in `dat` are factors, convert them if necessary
  if (!all(unlist(lapply(dat, is.factor)))) {
    dat <- dat[, lapply(.SD, as.factor)]
  }

  # Validate prior and method options
  conj_prior <- match.arg(conj_prior, several.ok = FALSE)
  if (conj_prior %in% c("flat.prior") & is.null(alpha)) {
    stop("Please supply argument alpha as prior.") # Error if alpha is required but not provided
  }
  method <- match.arg(method, several.ok = FALSE)

  mc <- match.call() # Capture the call to this function
  p <- ncol(dat) # Get the number of columns in the data

  # Perform imputation based on the selected method
  if(method == "EM"){
    # Run Expectation-Maximization (EM) algorithm
    mle_multinomial <- EM_multinomial(dat,
                                      conj_prior = conj_prior,
                                      alpha = NULL, tol = 5e-7, max_iter = 10000)
  }
  else if (method == "DA"){
    # Run Dirichlet Allocation (DA) algorithm
    mle_multinomial <- DA_multinomial(dat,
                                      conj_prior = conj_prior,
                                      alpha = alpha, burnin = 100, post_draws = 10000)
  }

  #-------------------------------------------------------
  # Enumerate all possible combinations of factor levels
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
  enum_comp <- enum[stats::complete.cases(enum),] # Data without missing values
  enum_miss <- enum[!stats::complete.cases(enum),] # Data with missing values
  enum_miss <- enum_miss[apply(enum_miss, 1, function(x) !all(is.na(x))),] # Remove rows where all values are missing
  data.table::setDT(enum_comp) # Convert to data.table format
  rownames(enum_comp) <- seq_len(nrow(enum_comp)) # Assign row names

  # Get counts / sufficient statistics for MLE
  #-------------------------------------------------------
  dat_comp <- dat[stats::complete.cases(dat),] # Data with complete cases
  dat_miss <- dat[!stats::complete.cases(dat),] # Data with missing values

  print("Imputing missing observations via MLE results.")
  # Impute missing data using the MLE results
  dat_miss2 <- impute_multinomial_all(dat_miss, mle_multinomial@mle_x_y, p = p)

  # Combine complete data and imputed data
  imputed_data <- rbind(dat_comp, dat_miss2)

  # Return an imputeMulti object containing results
  ret <- methods::new("imputeMulti",
                      Gcall = mc, method = mle_multinomial@method,
                      mle_call = mle_multinomial@mle_call,
                      mle_iter = mle_multinomial@mle_iter,
                      mle_log_lik = mle_multinomial@mle_log_lik,
                      mle_cp = mle_multinomial@mle_cp,
                      mle_x_y = mle_multinomial@mle_x_y,
                      data = list(missing_data = dat_miss, imputed_data = imputed_data),
                      nmiss = nrow(dat_miss)
  )

  return(ret) # Return the result object
}
