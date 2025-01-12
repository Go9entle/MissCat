

# MissCat

MissCat is an R package developed as part of the graduate course in Advanced Statistical Computing at the School of Statistics, Renmin University of China. The package is designed to perform data imputation for missing categorical data using the Expectation-Maximization (EM) and Data-Augmentation (DA) methods. It is specifically tailored for handling missing values in categorical data and provides an efficient approach to data imputation. For performance, calculation of the summary statistics of the multinomial distribution is implemented in C++.

## Introduction

In the past several decades, widely available methods have been developed for missing value imputation allowing practitioners to move beyond ad-hoc methods such as casewise-deletion and mean imputation, to more advanced methods such as multiple imputation Rubin(1987), expectation maximization(EM) Dempster, Laird, and Rubin(1977), chained equations van Buuren and Groothuis-Oudshoorn(2011), and data augmentation(DA) Tanner and Wong(1987), However, many of these methods assume the data comes from a multivariate normal distribution, which ignores missing data from a variety of other distributions. `MissCat` performs missing data imputation for the common multivariate multinomial distribution. Like other imputation methods, this method creates a "filled in" version of the incomplete data so that analyses which require complete observations can appropriately use all the data in a dataset containing missingness. `MissCat` uses the familiar EM and DA algorithms to estimate the parameter estimates of the multinomial distribution and maximum likelihood to impute missing observations.

## Functionality

MissCat provides tools to impute missing data in categorical datasets using both EM and DA methods. The package allows for imputation in datasets with multiple missing values across several variables, enabling users to efficiently recover missing information and create complete datasets for further analysis.

## Installation

To install the `MissCat` package, you can use the following code:

```R
devtools::install_github("go9entle/MissCat")
```

> [!Warning]
>
> If you get the following error
>
> ```R
> > devtools::install_github("go9entle/MissCat")
> Using GitHub PAT from the git credential store.
> 错误: Failed to install 'MissCat' from GitHub:
>   HTTP error 401.
>   Bad credentials
> 
>   Rate limit remaining: 59/60
>   Rate limit reset at: 2025-01-12 04:03:15 UTC
> ```
>
> Go to [Github/settings/tokens](https://github.com/settings/tokens)
> Click "Generate new token." (classic probably the best)
> Select the appropriate scopes for your use case. For installing packages, the following scopes are usually enough:
>
> ```github
> repo
> read:packages
> ```
>
> Click "Generate token."
> Copy the token, as you'll need it soon.
>
> Set up the PAT in R:
>
> Open R and run the following commands to set the new token:
>
> ```R
> gitcreds::gitcreds_set()
> ```
>
> This will prompt you to enter a new credential. Paste your newly generated PAT when prompted.



## Example Data

The package includes an example dataset `tract2221`, which contains data from the **American Community Survey (ACS)** for individuals living in census tract 2221 in Los Angeles County, California. The dataset includes 10 variables with 3974 samples, some of which have missing values. This dataset is provided in the `data/tract2221` file.

```R
library("MissCat")
data("tract2221")
```

## Available Methods

The main methods for imputation in the `MissCat` package are:

- **EM (Expectation-Maximization)**: A widely used method for missing data imputation, based on the EM algorithm.
- **DA (Data Augmentation)**: A Bayesian method that uses Dirichlet distributions for imputation.

Both methods offer flexibility in how priors are handled, with options like `"non.informative"` priors available for controlling the imputation process.





## main Functions

- `impute4mmv`: Main imputation function for generating complete datasets using EM & DA methods.
- `impute5mmv`: Similar to `impute4mmv`, but imputes in the original variable order for comparison purposes.

## Example

```R
# Example usage of the impute4mmv function
library("MissCat")
data("tract2221")
dat <- tract2221[,c(1:2,4,7,9)]
result <- impute4mmv(dat, method="EM", conj_prior = "non.informative")

```

## License

MissCat is released under the [MIT License](LICENSE).

