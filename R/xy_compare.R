#' @title Compare Rows of Two Data Tables
#' @description Compares each row of `DT_x` with `DT_y`, returning the indices of matching rows in `DT_y`.
#'
#' This function compares two data.tables, `DT_x` and `DT_y`, and returns a list where each element contains
#' the row indices from `DT_y` that match the corresponding row in `DT_x`. It handles missing values in `DT_x`
#' by excluding columns with missing values during the comparison.
#'
#' @param DT_x A data.table representing the first dataset to compare.
#' @param DT_y A data.table representing the second dataset to compare with.
#'
#' @return A list of length equal to the number of rows in `DT_x`. Each element of the list contains
#'         the row indices from `DT_y` that match the corresponding row in `DT_x`.
#'         If a row in `DT_x` contains missing values, the function compares only the non-missing values.
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#' @examples
#' library(data.table)
#'
#' # Create example data.tables
#' DT_x <- data.table(A = c("a", "b", NA), B = c(1, 2, 3))
#' DT_y <- data.table(A = c("a", "b", "c"), B = c(1, 2, 4))
#'
#' # Compare rows
#' result <- mx_my_compare(DT_x, DT_y)
#' print(result)
#'
#' # Expected output:
#' # [[1]]
#' # [1] 1
#' #
#' # [[2]]
#' # [1] 2
#' #
#' # [[3]]
#' # integer(0)  # No matches due to NA in DT_x
#'
#' @export

mx_my_compare <- function(DT_x, DT_y) {
  if (ncol(DT_x) != ncol(DT_y)) stop("ncol of DT_x and DT_y do not match.")

  ## 0. Pre-processing: convert factors to integers
  data.table::setDT(DT_x); data.table::setDT(DT_y)
  res <- vector("list", length= nrow(DT_x))
  join_cols <- names(DT_x)
  DT_y[, rowid := .I]

  for (s in seq_len(nrow(DT_x))) {
    tmp <- DT_x[s,] #获取当前行的内容
    na_idx <- which(apply(tmp, 1, is.na)) #判断当前行是否有缺失值
    if (length(na_idx) > 0) { #若有一个及以上缺失值
      res[[s]] <- DT_y[tmp, on= join_cols[-na_idx]]$rowid  #按DT_y去掉DT_x中含有缺失值的列进行匹配
    }
    else {
      res[[s]] <- DT_y[tmp, on= join_cols]$rowid #没有缺失值则精准匹配
    }
  }

  DT_y[, rowid := NULL]
  return(res)
  #返回一个列表 res，每个元素是与 DT_x 每一行匹配的 DT_y 行的索引。
}
