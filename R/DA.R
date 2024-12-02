#' Dirichlet Allocation for Multinomial Data
#'
#' This function implements a Dirichlet Allocation model for handling
#' multinomial data with missing values using a Bayesian approach.
#'
#' @param dat A data frame containing the data with missing values. All columns
#' should be factors.
#' @param conj_prior A character string specifying the type of prior to use.
#' Options include "none", "data.dep", "flat.prior", and "non.informative".
#' @param alpha A numeric vector specifying the prior parameters. Required if
#' `conj_prior` is "data.dep" or "flat.prior".
#' @param burnin An integer specifying the number of burn-in iterations. Default is 100.
#' @param post_draws An integer specifying the number of posterior draws. Default is 10000.
#'
#' @return An object containing:
#' \item{theta_y}{Estimated parameters for the multinomial distribution.}
#' \item{log_lik}{Log-likelihood of the fitted model.}
#' \item{counts}{Counts of the observed patterns.}
#' \item{alpha}{Prior parameters used in the model.}
#'
#' @export
DA_multinomial <- function(dat,
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative"),
                           alpha= NULL, burnin= 100, post_draws = 10000
) {

  # check some errors
  conj_prior <- match.arg(conj_prior, several.ok= FALSE)
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }

  mc <- match.call()



  # 1. Data processing
  data.table::setDT(dat) #转化为data.table形式
  if (!all(unlist(lapply(dat, is.factor)))) {dat <- dat[, lapply(.SD, as.factor)]}
  #检查 dat 中的所有列是否为因子类型；若不是，将其转换为因子。

  #得到所有的分类组合/完整数据的分类组合/有缺失数据的分类组合
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA)))) #对dat中每一列
  enum_comp <- enum[stats::complete.cases(enum),]
  enum_miss <- enum[!stats::complete.cases(enum),]
  enum_miss <- enum_miss[apply(enum_miss, 1, function(x) !all(is.na(x))),] # not all missing

  data.table::setDT(enum_comp)
  rownames(enum_comp) <- seq_len(nrow(enum_comp))

  #对样本数据也分为完整与存在缺失
  dat_comp <- dat[stats::complete.cases(dat),]
  dat_miss <- dat[!stats::complete.cases(dat),]#只要样本有NA就归到这里

  enum_comp_int <- do.call("cbind",lapply(enum_comp, fact_to_int)) #转成数值形式的enum_comp,即所有组合的完整模式
  enum_miss_int <- do.call("cbind",lapply(enum_miss,fact_to_int))#所有组合的缺失模式的数值形式
  dat_miss_int <- do.call("cbind",lapply(dat_miss,fact_to_int)) #样本数据中缺失模式的数值形式

  #从这开始要想一下如何优雅地传入Rcpp函数!

  library(data.table)
  enum_comp_int1 <- as.data.table(enum_comp_int)

  z_Os_y0 <- data.table::setDT(merge_duplicate_rows_large(dat_miss_int))
  #m_d_r_c是个Rcpp函数,可以合并相同模式的样本数据,并在最后一列计数
  z_Os_y <- data.table::setDT(match_rows_and_add_rowname(z_Os_y0,enum_miss_int))
  z_Os_y1 <- z_Os_y[, rowname := as.integer(rownames(z_Os_y))]
  setorder(z_Os_y1, rowname)
  # 将排序后的行名还原为行名
  rownames(z_Os_y1) <- z_Os_y1$rowname
  z_Os_y <- z_Os_y1[, rowname := NULL] # 删除辅助列 #test <- rownames(x_y23) test2 <- rownames(x_y) 这俩一样,大功告成!
  #实际上这个行名称才是有用的!
  z_p <- ncol(z_Os_y)
  search_out <- mx_my_compare(z_Os_y[,1:5],enum_comp_int1)
  rm(z_Os_y0)
  rm(z_Os_y1)
  #去掉z中NA列 再和enum_comp匹配,输出一个列表,用z2的行数,每一行是x2中与其匹配的行号
  #可是这是要干什么?
  #在缺失数据场景下，寻找部分观测数据与可能补全数据之间的匹配。
  #这种匹配是为了在 E 步骤 中进行期望值计算（Expectation），以及在 M 步骤 中重新估计模型参数
  #search_z_Os_y 是为处理缺失数据提供的一种高效方法，确保 E 步骤中能正确找到匹配的完整数据组合，从而推动 EM 算法的收敛和参数优化。

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

  n_obs= nrow(dat)

  enum_comp <- check_prior(conj_prior= conj_prior, alpha= alpha, #加上最后两列alpha和theta
                           outer= FALSE, enum_comp= enum_comp)
  comp_ind <- search_out



  #if (verbose) print("Setting up Iteration 1.")
  # pattern match marginally missing to complete
  #no_col_XP <- which(names(enum_comp) %in% c("alpha", "theta_y"))
  #comp_ind <- search_z_Os_y(z_Os_y, enum_comp[,-no_col_XP, with= FALSE])

  # 02. I and P Steps
  #----------------------------------------------
  library(gtools)
  iter <- 0
  while (iter < burnin) {
    # I Step
    log_lik <- log_lik0 <- 0
    enum_comp$counts <- 0

    for (s in 1:nrow(z_Os_y)) {
      # random allocation of observed marginal counts to complete pattern y
      # (x_y| z_Os_y, theta) = \sum_s (Xsy|Zsy, gamma)
      # (Xsy|Zy_theta) ~ M(Zsy, gamma)
      b_Os_y <- sum(enum_comp$theta_y[unlist(comp_ind[[s]])])

      E_Xsy_Zy_theta <- as.vector(stats::rmultinom(1, size= z_Os_y$counts[s],
                                                   prob= enum_comp$theta_y[unlist(comp_ind[[s]])] / b_Os_y)) # normalized probability
      # expected count += random draw based on marginally-observed
      enum_comp$counts[unlist(comp_ind[[s]])] <- enum_comp$counts[unlist(comp_ind[[s]])] + E_Xsy_Zy_theta

      # update log-lik
      if (b_Os_y > 0) {
        log_lik <- log_lik + z_Os_y$counts[s] * log(b_Os_y)
      }
    }
    # expected count += observed counts
    enum_comp$counts[as.integer(rownames(x_y))] <- enum_comp$counts[as.integer(rownames(x_y))] + x_y$counts
    # update log-lik
    log_lik <- log_lik + sum(ifelse(enum_comp$theta_y[as.integer(rownames(x_y))] == 0, 0,
                                    x_y$counts * log(enum_comp$theta_y[as.integer(rownames(x_y))])))

    # P Step
    if (conj_prior == "none") {
      # in case of random zeros: use non-informative prior
      enum_comp$theta_y1 <- as.vector(gtools::rdirichlet(n=1, alpha= enum_comp$counts + 1))
    } else {
      enum_comp$theta_y1 <- as.vector(gtools::rdirichlet(n=1, alpha= enum_comp$counts + enum_comp$alpha))
    }

    ### update iteration; print likelihood if verbose
    iter <- iter + 1
    #if (verbose) {
    #  cat("Iteration", iter, ": log-likelihood =", sprintf("%.10f", log_lik), "... \n")
    #}

    enum_comp$theta_y <- enum_comp$theta_y1
    log_lik0 <- log_lik
  }

  # 03. if iter >= max_iter, exit
  # MLE for theta_y is taken to be the mean of n= post_draws draws from the
  # posterior distribution
  #----------------------------------------------
  # update log-lik for prior
  if (conj_prior != "none") {
    log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                    enum_comp$alpha * log(enum_comp$theta_y)))
  }

  if (conj_prior == "none") {
    # in case of random zeros: use non-informative prior
    theta_post <- gtools::rdirichlet(n= post_draws, alpha= enum_comp$counts + 1)
    enum_comp$theta_y <- colMeans(theta_post)
  } else {
    theta_post <- gtools::rdirichlet(n= post_draws, alpha= enum_comp$counts + enum_comp$alpha)
    enum_comp$theta_y <- colMeans(theta_post)
  }

  enum_comp$theta_y1 <- NULL
  enum_comp$counts <- NULL

  mod <- methods::new("mod_imputeMulti",
                      method= "DA",
                      mle_call= mc,
                      mle_iter= iter,
                      mle_log_lik= log_lik,
                      mle_cp= conj_prior,
                      mle_x_y= enum_comp)

  # mod <- list(method= "DA",
  #             mle_call= mc,
  #             mle_iter= iter,
  #             mle_log_lik= log_lik,
  #             mle_cp= conj_prior,
  #             mle_x_y= enum_comp)
  # class(mod) <- "mod_imputeMulti"

  return(mod)
}
