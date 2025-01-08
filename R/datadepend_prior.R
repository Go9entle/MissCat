#' @title Parameters of the data-dependent prior distribution
#' @description Creates a data-dependent prior for a p-dimensional multinomial distribution. Only referenced if conj_prior is specified.
#' Use a conjugate prior based on 20% of the data (e.g. \eqn{Dirichlet(\alpha)}).
#'
#' @param dat dataset
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#' @return The original data will have one more column at the end, which is the parameter of the prior distribution.
#' @export
#'
data_dep_prior_multi <- function(dat) {
  data.table::setDT(dat)
  if (!all(unlist(lapply(dat, is.factor)))) {
    # enforce factor variables
    dat <- dat[, lapply(.SD, as.factor)]
  }
  dat_comp <- dat[stats::complete.cases(dat),] # Complete data
  dat_miss <- dat[!stats::complete.cases(dat),] # Missing data
  dat_int <-  do.call("cbind",lapply(dat,fact_to_int))
  dat_comp_int<- do.call("cbind",lapply(dat_comp,fact_to_int))
  enum_comp1 <- expand.grid(sapply(dat, levels)) #记住此!原代码中这里定义成了enum导致一系列分析错误
  enum_comp1_int <- do.call("cbind",lapply(enum_comp1,fact_to_int))
  comp <- which(stats::complete.cases(dat))
  comp_frac <- length(comp) / nrow(dat)

  if (comp_frac < .2) { #如果数据稀疏性比较大!#这里有问题!这个count_levels是在干啥!
    prior <- data.table::setDT(merge_duplicate_rows_large(dat_comp_int))
    #prior <- count_levels(dat, enum_list= enum, hasNA= "no") #x_y算法很像,样本数据在所有组合中
  } else {#如果数据稀疏性不大
    n <- round(.2 * length(comp) / comp_frac, 0)
    samp <- sample(comp, size= n)
    dat1_int <- dat_int[samp,]
    prior <- data.table::setDT(merge_duplicate_rows_large(dat1_int))
    #prior <- count_levels(dat[samp,], enum_list= enum, hasNA= "no")
  }

  prior <- merge(enum_comp1_int, prior, all.x= TRUE, all.y=FALSE)
  names(prior)[ncol(prior)] <- "alpha" # naming convention of dirichlet prior
  prior$alpha <- ifelse(is.na(prior$alpha), 1, prior$alpha)
  #如果 prior$alpha 中有 NA 值（意味着某个因子组合在数据中没有出现过），
  #则将其替换为 1。这是 Dirichlet 先验中常见的做法，表示未观察到的组合的先验值是 1（平滑处理）
  #alpha最终通过check_p的False赋给enum最后一列??但是我看到只有enum_comp的alpha在EM中被调用啊
  return(data.table::setDT(prior))#这个最终会生成alpha啊啊啊!是一个enum多一列!
}
