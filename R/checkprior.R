#' @title Check Prior Distribution Parameters
#' @describeIn Generate the parameters of the prior distribution OR
#'       generate the posterior distribution from the parameters of the prior distribution.
#'
#' @param dat A dataset (usually of class `data.frame` or `data.table`) containing the data used to calculate the prior or posterior parameters.
#' @param conj_prior A string indicating the type of prior distribution to use. Can be one of:
#'        - "none": No prior distribution, generate random theta values.
#'        - "data.dep": A data-dependent prior (user-supplied or calculated from `dat`).
#'        - "flat.prior": A flat prior with a scalar alpha parameter.
#'        - "non.informative": A non-informative Jeffrey's prior (alpha = 1).
#' @param alpha A scalar or vector that defines the prior distribution. The exact behavior depends on `conj_prior`:
#'        - For "flat.prior", this should be a scalar.
#'        - For "data.dep", this will be either user-supplied or calculated from the data.
#'        - For "non.informative", this is ignored and set to 1.
#' @param outer A logical value (default is `FALSE`). If `TRUE`, the function is called within the `multinomial_impute` function, which controls how prior and posterior distributions are handled.
#' @param enum_comp A `data.table` or `data.frame` that stores enumeration components, which will be updated with prior/posterior parameters.
#'
#' @return If `outer` is `TRUE`, returns the prior (alpha) as a scalar or vector, depending on the type of prior selected.
#'         If `outer` is `FALSE`, the function returns `enum_comp` with two additional columns:
#'         - The parameters of the prior distribution (`alpha` or calculated values).
#'         - The parameters of the posterior distribution (`theta_y`).
#' @importFrom Rcpp sourceCpp
#' @useDynLib MissCat
#' @export
check_prior <- function(dat, conj_prior = c("none", "data.dep", "flat.prior", "non.informative"),
                        alpha = NULL,
                        outer = FALSE, enum_comp = NULL) {

  if (outer) { # called within multinomial_impute, calculating prior or posterior
    if (conj_prior == "none") return(NULL) # No prior distribution, return NULL

    if (conj_prior == "data.dep") {
      # Use data-dependent prior if alpha is NULL, calculate alpha from data
      if (!is.null(alpha)) { # User supplied alpha
        message("Using user-supplied data dependent prior.")
      } else { # Calculate alpha from data
        message("Calculating data dependent prior.")
        alpha <- data_dep_prior_multi(dat = dat) # Function to calculate alpha based on data
      }
    } else if (conj_prior == "flat.prior") {
      # For flat prior, alpha should be a scalar
      if (!(is.vector(alpha) & length(alpha) == 1)) {
        stop("Flat priors must be supplied as a scalar.")
      }
      alpha <- alpha
    } else if (conj_prior == "non.informative") {
      # For non-informative prior, set alpha to 1 (Jeffrey's prior)
      alpha <- 1 # Jeffrey's prior * 2
    }
    return(alpha) # Return alpha value based on prior type

  } else { # called within multinomial_em or multinomial_data_aug, handle posterior calculation

    if (conj_prior != "none") {
      if (conj_prior == "data.dep") {
        # Ensure the number of rows in alpha matches the number of rows in enum_comp
        if (nrow(alpha) != nrow(enum_comp)) {
          stop("nrow(alpha) must match nrow(enum_comp).")
        }
        # Merge alpha into enum_comp
        enum_comp <- merge(enum_comp, alpha, by = "row.names", all.x = TRUE)
      } else if (conj_prior == "flat.prior") {
        # For flat prior, assign the same alpha to all rows in enum_comp
        if (!(is.vector(alpha) & length(alpha) == 1)) {
          stop("Flat priors must be supplied as a scalar.")
        }
        enum_comp$alpha <- alpha
      } else if (conj_prior == "non.informative") {
        # For non-informative prior, set alpha to 1
        enum_comp$alpha <- 1 # Jeffrey's prior * 2
      }

      # Calculate theta_y from alpha
      enum_comp$theta_y <- enum_comp$alpha / sum(enum_comp$alpha)
    } else { # If prior is "none", generate random theta values
      enum_comp$theta_y <- stats::runif(nrow(enum_comp))
      enum_comp$theta_y <- enum_comp$theta_y / sum(enum_comp$theta_y) # Normalize theta_y
    }

    # Return enum_comp with updated alpha and theta_y columns
    return(data.table::setDT(enum_comp))
  }
}
