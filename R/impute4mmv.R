#' Impute Missing Values for Multinomial Data
#'
#' This function imputes missing values in categorical data using either
#' Expectation-Maximization (EM) or Dirichlet Allocation (DA) methods.
#' It utilizes prior distributions for the imputation process.
#'
#' @param dat A data.table or data frame containing the data with missing values.
#' All columns should be factors.
#' @param method A character string specifying the imputation method.
#' Options include "EM" for Expectation-Maximization and "DA" for Dirichlet Allocation.
#' @param conj_prior A character string specifying the type of prior to use.
#' Options include "none", "data.dep", "flat.prior", and "non.informative".
#' @param alpha A numeric vector specifying the prior parameters. Required if
#' `conj_prior` is "flat.prior".
#'
#' @return An object of class `imputeMulti` containing:
#' \item{Gcall}{The call to the function.}
#' \item{method}{The method used for imputation.}
#' \item{mle_call}{The call used for maximum likelihood estimation.}
#' \item{mle_iter}{The number of iterations in MLE.}
#' \item{mle_log_lik}{Log-likelihood of the fitted model.}
#' \item{mle_cp}{Conjugate prior used.}
#' \item{mle_x_y}{Estimated parameters for the multinomial distribution.}
#' \item{data}{A list containing the original missing data and the imputed data.}
#' \item{nmiss}{The number of missing observations.}
#'
#' @export
impute4mmv <- function(dat, method= c("EM", "DA"),
                       conj_prior= c("none", "data.dep", "flat.prior", "non.informative"),
                       alpha= NULL){
  set.seed(2024)
  data.table::setDT(dat)
  if (!all(unlist(lapply(dat, is.factor)))) {dat <- dat[, lapply(.SD, as.factor)]} #检查 dat 中的所有列是否为因子类型；若不是，将其转换为因子。

  conj_prior <- match.arg(conj_prior, several.ok= FALSE) #确认 conj_prior 和 method 参数的值有效（匹配选项）。
  if (conj_prior %in% c("flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }
  #  如果选择了 flat.prior，但未提供 alpha 参数，则抛出错误。
  method <- match.arg(method, several.ok= FALSE)

  mc <- match.call()
  p <- ncol(dat)


  if(method == "EM"){
    mle_multinomial <- EM_multinomial(dat,
                                      conj_prior=conj_prior,
                                      alpha = NULL, tol = 5e-7, max_iter = 10000)
  }

  else if (method == "DA"){
    mle_multinomial <- DA_multinomial(dat,
                                      conj_prior= conj_prior,
                                      alpha= alpha, burn = 100, post_draws = 10000)
  }

  #----------------------------------------------
  # EM & DA
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA)))) #对dat中每一列
  enum_comp <- enum[stats::complete.cases(enum),]
  enum_miss <- enum[!stats::complete.cases(enum),]
  enum_miss <- enum_miss[apply(enum_miss, 1, function(x) !all(is.na(x))),] # not all missing
  data.table::setDT(enum_comp)
  rownames(enum_comp) <- seq_len(nrow(enum_comp))

  # 02. get counts / sufficient statistics
  #   parse / compute prior
  #----------------------------------------------
  dat_comp <- dat[stats::complete.cases(dat),]
  dat_miss <- dat[!stats::complete.cases(dat),]

  print("Imputing missing observations via MLE results.")
  dat_miss2 <- impute_multinomial_all(dat_miss, mle_multinomial@mle_x_y, p=p)
  #调用 impute_multinomial_all 函数，根据估计参数插补缺失值，将插补后的数据与完整数据合并。

  #combine:
  imputed_data <- rbind(dat_comp, dat_miss2)

  ret <- methods::new("imputeMulti",
                      Gcall= mc, method= mle_multinomial@method,
                      mle_call= mle_multinomial@mle_call,
                      mle_iter= mle_multinomial@mle_iter,
                      mle_log_lik= mle_multinomial@mle_log_lik,
                      mle_cp= mle_multinomial@mle_cp,
                      mle_x_y= mle_multinomial@mle_x_y,
                      data= list(missing_data= dat_miss, imputed_data= imputed_data),
                      nmiss= nrow(dat_miss)
  )

  return(ret)


}
