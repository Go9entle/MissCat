#' @title Data-Augmentation for Multinomial Data with Missing Values
#' @description This function implements a Data Augmentation (DA) model for handling multinomial
#' data with missing values using a Bayesian approach. The method applies an
#' Iterative Markov Chain Monte Carlo (MCMC) algorithm to estimate the parameters
#' of a multinomial distribution with Dirichlet priors. The model proceeds with
#' an initial iteration step (I-step) followed by the maximization step (M-step)
#' and continues until a specified number of burn-in iterations. Afterward, the model
#' performs posterior draws to estimate the parameters of the multinomial distribution.
#' @import data.table
#' @import gtools
#' @import stats
#' @import methods
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#' @param dat A data frame containing the data with missing values. All columns
#'            should be factors. The missing values are assumed to be missing at random (MAR).
#' @param conj_prior A character string specifying the type of prior to use for the Dirichlet distribution.
#'                   Options include:
#'        - "none": No prior distribution is used (non-informative).
#'        - "data.dep": Data-dependent prior.
#'        - "flat.prior": A flat (uniform) prior is used.
#'        - "non.informative": A non-informative Jeffrey's prior.
#' @param alpha A numeric vector specifying the prior parameters. Required if
#'              `conj_prior` is "data.dep" or "flat.prior". Represents the
#'              parameters of the Dirichlet prior distribution.
#' @param burnin An integer specifying the number of burn-in iterations. Default is 100.
#'               These iterations are used to stabilize the parameter estimates before sampling.
#' @param post_draws An integer specifying the number of posterior draws. Default is 10000.
#'                   These draws are used to estimate the parameters of the multinomial distribution.
#'
#' @return An object of class `mod_imputeMulti` containing:
#' \item{theta_y}{Estimated parameters for the multinomial distribution, obtained through posterior draws.}
#' \item{log_lik}{The log-likelihood of the fitted model based on the posterior draws.}
#' \item{counts}{Counts of the observed patterns in the dataset, used in the likelihood calculation.}
#' \item{alpha}{Prior parameters used in the model, if applicable.}
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data("tract2221")
#' dat <- tract2221[,c(1:2,4,7,9)]
#' result <- DA_multinomial(dat, conj_prior = "non.informative",
#'                          burnin = 100, post_draws = 10000)
DA_multinomial <- function(dat,
                           conj_prior = c("none", "data.dep", "flat.prior", "non.informative"),
                           alpha = NULL, burnin = 100, post_draws = 10000) {

  # Validate prior options and handle missing alpha parameter if required
  conj_prior <- match.arg(conj_prior, several.ok = FALSE) # Match the prior choice
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha)) {
    stop("Please supply argument alpha as prior.") # If alpha is missing, stop execution
  }

  mc <- match.call() # Capture the function call for the object
  p <- ncol(dat) # Number of columns (variables) in the dataset
  n_obs <- nrow(dat) # Number of observations in the dataset

  # Step 1: Data Preprocessing
  data.table::setDT(dat) # Convert to data.table for efficiency
  if (!all(unlist(lapply(dat, is.factor)))) {
    dat <- dat[, lapply(.SD, as.factor)] # Ensure all columns are factors
  }

  # Create all possible combinations of factor levels for complete and missing data
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
  enum_comp <- enum[stats::complete.cases(enum), ]  # Complete cases
  enum_miss <- enum[!stats::complete.cases(enum), ] # Incomplete cases with missing values
  enum_miss <- enum_miss[apply(enum_miss, 1, function(x) !all(is.na(x))), ] # Remove rows where all values are NA

  data.table::setDT(enum_comp)
  rownames(enum_comp) <- seq_len(nrow(enum_comp)) # Set rownames for easy referencing

  # Split dataset into complete and incomplete data
  dat_comp <- dat[stats::complete.cases(dat),] # Complete data
  dat_miss <- dat[!stats::complete.cases(dat),] # Missing data

  # Convert to integer representation for efficient computation
  enum_comp_int <- do.call("cbind", lapply(enum_comp, fact_to_int))
  enum_miss_int <- do.call("cbind", lapply(enum_miss, fact_to_int))
  dat_miss_int <- do.call("cbind", lapply(dat_miss, fact_to_int))

  # Efficiently match incomplete data with complete data
  enum_comp_int1 <- as.data.table(enum_comp_int)
  z_Os_y0 <- data.table::setDT(merge_duplicate_rows_large(dat_miss_int))
  z_Os_y <- data.table::setDT(match_rows_and_add_rowname(z_Os_y0, enum_miss_int))
  z_Os_y1 <- z_Os_y[, rowname := as.integer(rownames(z_Os_y))]
  setorder(z_Os_y1, rowname)
  rownames(z_Os_y1) <- z_Os_y1$rowname
  z_Os_y <- z_Os_y1[, rowname := NULL]
  search_out <- mx_my_compare(z_Os_y[, 1:p], enum_comp_int1)
  rm(z_Os_y0, z_Os_y1)

  dat_comp_int <- do.call("cbind",lapply(dat_comp,fact_to_int))#对样本数据完整模式的转化为数值矩阵
  x_y0 <- data.table::setDT(merge_duplicate_rows_large(dat_comp_int)) #合并重复的列 并计数
  #下面将给x_y22匹配到所有组合完整数据中一个行号,根据行号排序!方便索引完整数据的先验alpha和参数theta_y
  x_y <- data.table::setDT(match_rows_and_add_rowname(x_y0,enum_comp_int))

  #给完rownames后我们将这一列加到x_y最后然后根据行号从小到大排序!
  x_y[, rowname := as.integer(rownames(x_y))]
  setorder(x_y, rowname)
  # 将排序后的行名还原为行名
  rownames(x_y) <- x_y$rowname
  x_y[, rowname := NULL] # 删除辅助列 #test <- rownames(x_y23) test2 <- rownames(x_y) 这俩一样,大功告成!
  #实际上这个行名称才是有用的!
  #test <- rownames(x_y)

  enum_comp <- check_prior(conj_prior= conj_prior, alpha= alpha, #加上最后两列alpha和theta
                           outer= FALSE, enum_comp= enum_comp)
  comp_ind <- search_out
  # Step 2: Iterative (I-step) and Maximization (M-step) of Data Augmentation Algorithm
  iter <- 0
  while (iter < burnin) {
    # I-step: Calculate the expected counts and log-likelihood
    log_lik <- log_lik0 <- 0
    enum_comp$counts <- 0

    # Loop over missing rows and update counts
    for (s in 1:nrow(z_Os_y)) {
      b_Os_y <- sum(enum_comp$theta_y[unlist(comp_ind[[s]])])
      E_Xsy_Zy_theta <- as.vector(stats::rmultinom(1, size = z_Os_y$counts[s],
                                                   prob = enum_comp$theta_y[unlist(comp_ind[[s]])] / b_Os_y))
      enum_comp$counts[unlist(comp_ind[[s]])] <- enum_comp$counts[unlist(comp_ind[[s]])] + E_Xsy_Zy_theta

      if (b_Os_y > 0) {
        log_lik <- log_lik + z_Os_y$counts[s] * log(b_Os_y)
      }
    }

    # Add observed counts to expected counts
    enum_comp$counts[as.integer(rownames(x_y))] <- enum_comp$counts[as.integer(rownames(x_y))] + x_y$counts
    log_lik <- log_lik + sum(ifelse(enum_comp$theta_y[as.integer(rownames(x_y))] == 0, 0,
                                    x_y$counts * log(enum_comp$theta_y[as.integer(rownames(x_y))])))

    # M-step: Maximize the log-likelihood by updating parameters
    if (conj_prior == "none") {
      enum_comp$theta_y1 <- as.vector(gtools::rdirichlet(n = 1, alpha = enum_comp$counts + 1)) # Non-informative prior
    } else {
      enum_comp$theta_y1 <- as.vector(gtools::rdirichlet(n = 1, alpha = enum_comp$counts + enum_comp$alpha)) # Informative prior
    }

    iter <- iter + 1
    enum_comp$theta_y <- enum_comp$theta_y1
    log_lik0 <- log_lik
  }

  # Step 3: Posterior draws and parameter estimation
  if (conj_prior != "none") {
    log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                    enum_comp$alpha * log(enum_comp$theta_y)))
  }

  # Generate posterior samples for parameter estimation
  if (conj_prior == "none") {
    theta_post <- gtools::rdirichlet(n = post_draws, alpha = enum_comp$counts + 1)
    enum_comp$theta_y <- colMeans(theta_post)
  } else {
    theta_post <- gtools::rdirichlet(n = post_draws, alpha = enum_comp$counts + enum_comp$alpha)
    enum_comp$theta_y <- colMeans(theta_post)
  }

  enum_comp$theta_y1 <- NULL
  enum_comp$counts <- NULL

  # Return the results as an object of class 'mod_imputeMulti'
  mod <- methods::new("mod_imputeMulti",
                      method = "DA",
                      mle_call = mc,
                      mle_iter = iter,
                      mle_log_lik = log_lik,
                      mle_cp = conj_prior,
                      mle_x_y = enum_comp)

  return(mod)
}
