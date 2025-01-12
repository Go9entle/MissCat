#' @title Impute Missing Values Across All Rows in Multinomial Data
#' @description This function imputes missing values for all rows in a dataset using the
#' `impute_multinomial` function based on the provided `MLEx_y` data.
#'
#' @param dat_miss A data frame with missing values to be imputed.
#' @param MLEx_y A data frame containing maximum likelihood estimates for imputation.
#' @param p An integer specifying the number of columns in `MLEx_y` to consider.
#'
#' @return A data frame with all missing values imputed.
#' @export
impute_multinomial_all <- function(dat_miss, MLEx_y, p) {
  gc()
  marg_ind <- mx_my_compare(dat_miss, MLEx_y[, 1:p])

  for (i in 1:nrow(dat_miss)) {
    dat_miss[i,] <- impute_multinomial(dat_miss[i,], row_ind= marg_ind[[i]], MLEx_y= MLEx_y)
  }

  return(dat_miss)
}
