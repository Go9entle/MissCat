#' Expectation-Maximization Algorithm for Multinomial Data
#'
#' This function implements the Expectation-Maximization (EM) algorithm to
#' estimate parameters for multinomial data with missing values.
#'
#' @param dat A data frame containing the data with missing values. All columns
#' should be factors.
#' @param conj_prior A character string specifying the type of prior to use.
#' Options include "none", "data.dep", "flat.prior", and "non.informative".
#' @param alpha A numeric vector specifying the prior parameters. Required if
#' `conj_prior` is "data.dep" or "flat.prior".
#' @param tol A numeric value indicating the convergence tolerance. Default is
#' 5e-7.
#' @param max_iter An integer specifying the maximum number of iterations.
#' Default is 10000.
#'
#' @return An object of class `mod_imputeMulti` containing:
#' \item{method}{The method used for imputation.}
#' \item{mle_call}{The call that produced the model.}
#' \item{mle_iter}{The number of iterations run.}
#' \item{mle_log_lik}{The log-likelihood of the fitted model.}
#' \item{mle_cp}{The conjugate prior used.}
#' \item{mle_x_y}{A data frame with estimated parameters and counts.}
#'
#' @export
EM_multinomial <- function(dat,
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative"),
                           alpha= NULL, tol= 5e-7, max_iter= 10000
) {

  # 0. Check some errors
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

  #这一段插入A_auxiliaryf中的函数

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

  search_out <- mx_my_compare(z_Os_y[,1:5],enum_comp_int1)#找到search表！
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

  #这一段插入auxiliary

  n_obs= nrow(dat)

  enum_comp <- check_prior(conj_prior= conj_prior, alpha= alpha, #加上最后两列alpha和theta
                           outer= FALSE, enum_comp= enum_comp)
  comp_ind <- search_out


  # 2 E and M Steps
  #----------------------------------------------
  iter <- 0
  while (iter < max_iter) {
    # E Step
    log_lik <- log_lik0 <- 0
    enum_comp$counts <- 0

    for (s in 1:nrow(z_Os_y)) {
      # allocate observed marginal counts proportionally to complete patterns
      # E(x_y| z_Os_y, theta) = \sum_s [E_Xsy_Zy_theta]
      # 其中 E_Xsy_Zy_theta = (z_Os_y * theta_y) / b_Os_y
      b_Os_y <- sum(enum_comp$theta_y[unlist(comp_ind[[s]])])
      #b_Os_y 是与当前行 s 相关的权重总和，表示该行的所有可能组件的权重。

      # normalize
      E_Xsy_Zy_theta <- z_Os_y$counts[s] * enum_comp$theta_y[unlist(comp_ind[[s]])] / b_Os_y
      #计算出在给定观察到的模式和模型参数下，期望的计数值。
      #它是通过将观察到的计数按组件的权重进行比例分配，并归一化得到的。

      # expected count += proportional marginally-observed
      enum_comp$counts[unlist(comp_ind[[s]])] <- enum_comp$counts[unlist(comp_ind[[s]])] +
        E_Xsy_Zy_theta #一开始是0，根据theta_y来算counts——样本分布
      # 在算对数似然函数 log-lik
      if (b_Os_y > 0) {
        log_lik <- log_lik + z_Os_y$counts[s] * log(b_Os_y)
      }
    }
    #前面根据先验信息计算的样本分布是关于缺失z_Os_y的样本分布
    #为什么根据先验信息计算完样本分布后要加上观测到的计数？
    #所有被观测到的也算一种缺失模式
    # expected count += observed counts
    enum_comp$counts[as.integer(rownames(x_y))] <- enum_comp$counts[as.integer(rownames(x_y))] +
      x_y$counts
    # update log-lik
    log_lik <- log_lik + sum(
      ifelse(
        enum_comp$theta_y[as.integer(rownames(x_y))] == 0, 0,
        x_y$counts * log(enum_comp$theta_y[as.integer(rownames(x_y))])
      )
    )

    # M Step
    if (conj_prior == "none") {
      enum_comp$theta_y1 <- enum_comp$counts / n_obs
    } else {
      D <- nrow(enum_comp)
      alpha_0 <- sum(enum_comp$alpha)
      enum_comp$theta_y1 <- (enum_comp$counts + enum_comp$alpha - 1) / (n_obs + alpha_0 - D)
    }

    # update iteration

    iter <- iter + 1

    # 3 check convergence to exit and return
    #-----------------------------------------
    if (supDist(enum_comp$theta_y, enum_comp$theta_y1) < tol |
        abs(log_lik - log_lik0) < tol * 100) {
      # update log-lik for prior
      if (conj_prior != "none") {
        log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                        enum_comp$alpha * log(enum_comp$theta_y)))
      }
      enum_comp$theta_y1 <- NULL #将临时变量置为空
      enum_comp$counts <- NULL #将计数置为空

      mod <- methods::new("mod_imputeMulti", #这是定义的新class
                          method= "EM",
                          mle_call= mc,
                          mle_iter= iter,
                          mle_log_lik= log_lik,
                          mle_cp= conj_prior,
                          mle_x_y= enum_comp)

      return(mod)

    } else { #收敛条件不成立,则继续迭代
      enum_comp$theta_y <- enum_comp$theta_y1 #更新 theta换成更新完的的theta_y1
      log_lik0 <- log_lik
    }
  }  #while不成立!则超出最大迭代次数,直接返回
  # 4 iter>max_iter exit
  # update log-lik for prior
  if (conj_prior != "none") {
    log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                    enum_comp$alpha * log(enum_comp$theta_y)))
  }
  enum_comp$theta_y1 <- NULL
  enum_comp$counts <- NULL

  mod <- methods::new("mod_imputeMulti",
                      method= "EM",
                      mle_call= mc,
                      mle_iter= iter,
                      mle_log_lik= log_lik,
                      mle_cp= conj_prior,
                      mle_x_y= enum_comp)

  return(mod)
}
