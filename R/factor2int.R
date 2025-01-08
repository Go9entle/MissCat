#' @title Convert Factor Levels to Positive Integers
#'
#' @description
#' This function converts the levels of a factor variable to corresponding positive integers.
#' Each level of the factor is mapped to its integer position in the factor's levels.
#'
#' @param f A factor variable (or a column of factors) to be converted. If `f` is not a factor, it is returned unchanged.
#'
#' @return A numeric vector where each factor level is replaced by its corresponding integer position.
#'         Missing values (NA) are retained as NA.
#'
#' @details
#' This function is useful for converting categorical variables (factors) into numeric representations
#' when such transformation is required, for instance, in statistical or machine learning models that
#' need numeric inputs. It returns a numeric vector, where the factor levels are replaced with their
#' integer codes (starting from 1 for the first level).
#'
#' @export
#'
#' @examples
#' # Example usage:
#' factor_var <- factor(c("Low", "Medium", "High", "Low"))
#' fact_to_int(factor_var)
#' # Returns: [1, 2, 3, 1]
fact_to_int <- function(f) {
  if (is.factor(f)) {
    l <- levels(f) # Get the levels of the factor
    return(unlist(
      sapply(f, function(i) {
        ifelse(!is.na(i), which(i == l), NA)
        # If i is not NA, return the index of i in the factor levels
      })
    ))
  } else {
    return(f) # If not a factor, return the variable as is
  }
}
