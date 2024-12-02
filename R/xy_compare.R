#' @describeIn Compares two 2D arrays (\code{mat_x}, \code{mat_y}), where \code{mat_x} allows missing values. Returns a \code{list} of length \code{nrow(mat_x)} where each list element contains a vector of row indices from \code{mat_y} and the row equivalence of the non-missing values.
#'
#' @param DT_x
#' @param DT_y
#'
#' @return A list with the same number of rows as \code{DT_x}, each row containing the row indices where \code{DT_y} can be paired with \code{DT_x}.
#' @export
#'
#' @examples
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
