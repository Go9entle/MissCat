#' @describeIn Generate the parameters of the prior distribution OR
#'       generate the posterior distribution from the parameters of the prior distribution.
#'
#' @param dat
#' @param conj_prior
#' @param alpha
#' @param outer
#' @param enum_comp
#'
#' @return alpha OR The input data has two more columns, and the last two columns are the parameters of the prior distribution and the parameters of the posterior distribution.
#' @export
#'
#' @examples
check_prior <- function(dat, conj_prior= c("none", "data.dep", "flat.prior", "non.informative"),
                        alpha= NULL,
                        outer= FALSE, enum_comp= NULL) {
  if (outer) { # called w/in multinomial_impute
    if (conj_prior == "none") return(NULL)

    if (conj_prior == "data.dep") {
      if (!is.null(alpha)) { #如果不缺失alpha也就是用户指定了alpha
        #if (verbose == TRUE) print("Using user-supplied data dependent prior.")
        message("Using user-supplied data dependent prior.")
      } else {#用户没给定alpha,自行计算
        #if (verbose == TRUE) print("Calculating data dependent prior.")
        message("Calculating data dependent prior.")
        alpha <- data_dep_prior_multi(dat= dat)#?这不是enum每一行都会有吗?
      }
    } else if (conj_prior == "flat.prior") {
      if (!(is.vector(alpha) & length(alpha) == 1)) { #确保alpha是维度1的向量!
        stop("Flat priors must be supplied as a scalar.")
      }
      alpha <- alpha
    } else if (conj_prior == "non.informative") {
      alpha <- 1 # Jeffrey's prior * 2
    }
    return(alpha)#True的话计算alpha
  } else { # called w/in multinomial_em or multinomial_data_aug,outer是个开关?False的时候执行下面这一步,在em和da里用到
    if (conj_prior != "none") {
      if (conj_prior == "data.dep") {
        if (nrow(alpha) != nrow(enum_comp)) {#这怎么不行??alpha通过merge不是enum每一行都会有吗?
          stop("nrow(alpha) must match nrow(enum_comp).")#
        }
        enum_comp <- merge(enum_comp, alpha) #果然!
      } else if (conj_prior == "flat.prior") {
        if (!(is.vector(alpha) & length(alpha) == 1)) {
          stop("Flat priors must be supplied as a scalar.")
        }
        enum_comp$alpha <- alpha
      } else if (conj_prior == "non.informative") {
        enum_comp$alpha <- 1 # Jeffrey's prior * 2
      }
      # calc theta_y from alpha
      enum_comp$theta_y <- enum_comp$alpha / sum(enum_comp$alpha)
    } else { #是none才如下算法
      enum_comp$theta_y <- stats::runif(nrow(enum_comp))
      enum_comp$theta_y <- enum_comp$theta_y / sum(enum_comp$theta_y)
    }
    return(data.table::setDT(enum_comp))
  }#False的话将alpha和theta合并到enum_comp后两列
}
