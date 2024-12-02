#' Impute Missing Values in Multinomial Data
#'
#' This function imputes missing values in a single row of multinomial data using
#' maximum likelihood estimates based on the provided `MLEx_y` data.
#'
#' @param miss_val A data frame or vector containing missing values to be imputed.
#' @param row_ind An integer index indicating the row in `MLEx_y` to use for imputation.
#' @param MLEx_y A data frame containing maximum likelihood estimates for imputation.
#'
#' @return A data frame with missing values imputed.
#' @export
impute_multinomial <- function(miss_val, row_ind, MLEx_y) {
  ml_vals <- MLEx_y[row_ind,]
  ml_vals <- ml_vals[which.max(ml_vals$theta_y),]

  miss_val <- data.frame(miss_val)
  if (nrow(ml_vals) == 1) {
    miss_val[,which(is.na(miss_val))] <- ml_vals[which(is.na(miss_val))]
  } else {
    imp <- ml_vals[sample.int(nrow(ml_vals), size= 1),]
    miss_val[which(is.na(miss_val))] <- imp[which(is.na(miss_val))]
  }
  return(data.table::setDT(miss_val))
}
