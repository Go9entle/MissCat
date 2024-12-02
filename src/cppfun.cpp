#include <Rcpp.h>
#include <unordered_map>
#include <sstream>
#include <vector>
#include <algorithm>
using namespace Rcpp;



//' @title Match Rows Between Two DataFrames with Missing Data
 //' @description For each row in `z`, find matching rows in `x` after removing columns
 //' where `z` has missing values (NA).
 //' @param z A DataFrame containing missing data. Rows in `z` may have NA values.
 //' @param x A DataFrame without missing data. Rows in `x` are compared to `z`.
 //' @return A List where each element contains the indices of matching rows in `x` for each row in `z`.
 //' @export
 // [[Rcpp::export]]
 
 List match_rows(DataFrame z, DataFrame x) {//这个函数是生成list search那个!
   int nrow_z = z.nrow(); // z_Os_y的行数
   int nrow_x = x.nrow(); // x_possible的行数
   int ncol = z.ncol();   // z_的列数=x_的列数
   
   // 确保每列名称相同,列数相等
   //if (z.names() != x.names()) {
   //  stop("Column names of z and x must match.");
   //}
   if (z.ncol() != x.ncol()) {
     stop("z and x must have the same number of columns.");
   }
   
   // 创建一个列表用于存放最终的结果
   List result(nrow_z);
   
   // 首先对z的每行迭代
   for (int i = 0; i < nrow_z; i++) {
     // Extract the current row of z
     CharacterVector row_z(ncol); // 用于存储 z 的第 i 行
     for (int col = 0; col < ncol; col++) {
       CharacterVector z_col = z[col];
       row_z[col] = z_col[i];
     }
     
     
     // 找到不存在NA的列,称之为有效列
     std::vector<int> valid_cols;
     for (int col = 0; col < ncol; col++) { //第i行,对第col列查其是否为NA
       if (!CharacterVector::is_na(row_z[col])) { //如果不是NA,则我们认定这是有效列
         valid_cols.push_back(col);
       }
     }
     
     // If all columns are NA, no comparison can be made
     if (valid_cols.empty()) {
       result[i] = IntegerVector::create(); // Empty result
       continue;
     }
     
     // 找匹配
     std::vector<int> matches;
     for (int j = 0; j < nrow_x; j++) {
       bool match = true;
       for (int col : valid_cols) { //col取遍有效列valid_cols这个向量
         CharacterVector z_col = z[col];
         CharacterVector x_col = x[col];
         if (z_col[i] != x_col[j]) {
           match = false;
           break;
         }
       }
       if (match) {
         matches.push_back(j + 1); // R 索引从 1 开始
       }
     }
     
     // Store the matching indices
     result[i] = matches;
   }
   
   return result;
 }






// Helper function to convert a row to a hashable string
std::string row_to_string(const DataFrame& df, int row) {
  std::ostringstream oss;
  int ncol = df.size();
  for (int col = 0; col < ncol; ++col) {
    NumericVector col_data = df[col];
    if (NumericVector::is_na(col_data[row])) {
      oss << "NA"; // Use a special marker for NA
    } else {
      oss << col_data[row];
    }
    oss << ","; // Delimiter
  }
  return oss.str();
}

// [[Rcpp::export]]
DataFrame merge_duplicate_rows_large(DataFrame df) {
  int nrow = df.nrows();
  int ncol = df.size();
  
  // HashMap to store unique rows and their counts
  std::unordered_map<std::string, std::pair<std::vector<double>, int>> row_map;
  
  for (int i = 0; i < nrow; ++i) {
    // Convert row to hashable string
    std::string key = row_to_string(df, i);
    
    // Check if the row already exists in the map
    if (row_map.find(key) == row_map.end()) {
      // If new, add row data and initialize count to 1
      std::vector<double> row_data(ncol);
      for (int col = 0; col < ncol; ++col) {
        NumericVector col_data = df[col];
        row_data[col] = col_data[i];
      }
      row_map[key] = std::make_pair(row_data, 1);
    } else {
      // If exists, increment the count
      row_map[key].second++;
    }
  }
  
  // Create result DataFrame
  int unique_rows = row_map.size();
  NumericMatrix result(unique_rows, ncol + 1);
  CharacterVector col_names = clone<CharacterVector>(df.names());
  col_names.push_back("counts");
  
  int idx = 0;
  for (const auto& entry : row_map) {
    // Copy row data
    for (int col = 0; col < ncol; ++col) {
      result(idx, col) = entry.second.first[col];
    }
    // Add count as the last column
    result(idx, ncol) = entry.second.second;
    idx++;
  }
  
  // Convert to DataFrame and set column names
  DataFrame result_df = as<DataFrame>(result);
  result_df.attr("names") = col_names;
  return result_df;
}




// Helper function to convert a row to a hashable string
std::string row_to_string2(const DataFrame& df, int row, int ncol) {
  std::ostringstream oss;
  for (int col = 0; col < ncol; ++col) {
    NumericVector col_data = df[col];
    if (NumericVector::is_na(col_data[row])) {
      oss << "NA"; // Use a special marker for NA
    } else {
      oss << col_data[row];
    }
    oss << ","; // Delimiter
  }
  return oss.str();
}

// [[Rcpp::export]]
DataFrame match_rows_and_add_rowname(DataFrame A, DataFrame B) {
  int nrowA = A.nrows();
  int ncolA = A.size() - 1; // Exclude the last column for matching,这里是5
  int nrowB = B.nrows();
  int ncolB = B.size();
  
  if (ncolA > ncolB) {
    stop("DataFrame A (excluding last column) has more columns than DataFrame B. Matching is not possible.");
  }
  
  // Step 1: Create a temporary DataFrame tmpA without the last column of A
  DataFrame tmpA = A[Range(0, ncolA - 1)];//tmpA是前五列,没毛病
  
  // Step 2: Build a hash map for DataFrame B
  std::unordered_map<std::string, int> row_to_index_map;
  for (int i = 0; i < nrowB; ++i) {
    std::string key = row_to_string2(B, i, ncolA); // Use only first `ncolA` columns of B前五列 没毛病啊
    row_to_index_map[key] = i + 1; // Store 1-based row index 存储到B的行号
  }
  
  // Step 3: Find matches for DataFrame tmpA in B
  IntegerVector row_names(nrowA, NA_INTEGER); // Default to NA if no match is found
  for (int i = 0; i < nrowA; ++i) {
    std::string key = row_to_string2(tmpA, i, ncolA); // Convert tmpA's row to string
    auto it = row_to_index_map.find(key);
    if (it != row_to_index_map.end()) {
      row_names[i] = it->second; // Assign corresponding row index from B
    }
  }
  
  // Step 4: Add row names to original DataFrame A and return
  A.attr("row.names") = row_names; // Assign row names based on B's indices
  
  
  return A;
}


 //' @title Calculate the sup of L1 distance between x and y
 //' @description sup of L1 distance between x and y
 //' @param x A numeric \code{vector}
 //' @param y A numeric \code{vector}
 //' @return a numeric scalar.
 // [[Rcpp::export]]
 double supDist (const NumericVector& x, const NumericVector& y) {
   int nx = x.size();
   
   double sup = -1.0;
   for (int i = 0; i < nx; i++) {
     if (abs(x[i] - y[i]) > sup) sup = abs(x[i] - y[i]);
   }
   return sup;
 }



// [[Rcpp::export]]
IntegerVector count_compare (IntegerMatrix& x, IntegerMatrix& dat, const std::string& hasNA) {
  
  int nr_x = x.nrow(), nr_dat = dat.nrow(), nc_x = x.ncol();
  IntegerVector out(nr_x, 0);
  
  // Basic strategy: Loop through dat to find match in x. Each j in J has 1 match i in I
  // Once match is found, break and j++
  // match criteria differs by hasNA.
  // many to one matching (many dat.row(j) may match one x.row(i))
  
  if (hasNA == "no") {
    for (int j = 0; j < nr_dat; j++) {
      for (int i = 0; i < nr_x; i++) {
        if (is_true(all(x.row(i) == dat.row(j)))) {
          ++out[i];
          break; // x.row(i) are unique -- can only have one match
        }
      }
    }
  } else if (hasNA == "count.obs") {
    for (int j = 0; j < nr_dat; j++) {
      for (int i = 0; i < nr_x; i++) {
        // find observed values. Observed values in dat(j,) must match same values in x(i,)
        IntegerVector x_exist, dat_exist;
        for (int k = 0; k < nc_x; k++) {
          if (dat.row(j)[k] != NA_INTEGER) {
            dat_exist.push_back(dat.row(j)[k]);
            x_exist.push_back(x.row(i)[k]);
          }
        }
        if (is_true(all(x_exist == dat_exist))) {
          ++out[i];
          break; // x.row(i) are unique -- can only have one match
        }
      }
    }
  } else if (hasNA == "count.miss") {
    for (int j = 0; j < nr_dat; j++) { //对dat2的每一行
      for (int i = 0; i < nr_x; i++) { //找x的每一行去匹配
        // find NA values
        // split both rows into two vectors: one of indices of missing values and one of observed values
        // both must match for equivalence
        IntegerVector x_na, x_exist, dat_na, dat_exist;
        for (int k = 0; k < nc_x; k++) { //x中每一行逐列去匹配dat2中的(i,k)元素
          if (x.row(i)[k] == NA_INTEGER) {
            x_na.push_back(k); //记录缺失值索引：如果是缺失值，将其列索引 k 添加到 x_na 向量中。
          } else {
            x_exist.push_back(x.row(i)[k]);//否则添加这个观察值到x_exist
          }
          if (dat.row(j)[k] == NA_INTEGER) {//如果这个位置的dat是缺失值
            dat_na.push_back(k);
          } else {
            dat_exist.push_back(dat.row(j)[k]);
          }
        }
        // if (same number NA, same positions NA, same values existing) ==> match
        if (x_na.size() == dat_na.size() && is_true(all(x_na == dat_na)) &&
            is_true(all(x_exist == dat_exist))) {
          ++out[i];//缺失值数量、位置，观察值和数量都得相同
          break; // x.row(i) are unique -- can only have one match
        }
      }
    }
  } else {
    // cout << "ERROR: hasNA improperly specified" << endl;
    return -1;
  }
  // return counts
  return out;
}



