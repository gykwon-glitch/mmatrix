
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmatrix

<!-- badges: start -->

<!-- badges: end -->

mmatrix provides a safe, sparse, and reproducible formula-to-matrix
pipeline for high-dimensional data. It extends R’s base model.matrix()
workflow by offering transparent collinearity handling, stable factor
encoding, and detailed reporting of dropped or aliased columns. The
package centers on a main constructor that builds a sparse design matrix
and a frozen encoding specification (“spec”) containing factor levels,
contrasts, and transformation rules for consistent application to new
data.

## Installation

You can install the development version of mmatrix from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("gykwon-glitch/mmatrix")
```

## TO DO

1.  Finish making the code for mm_predict, considering using S3 class
    print instead of mm_report.
2.  (if time permits) do several tests or make vignettes

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mmatrix)
## basic example code
```
