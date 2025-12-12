
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmatrix

<!-- badges: start -->

<!-- badges: end -->

mmatrix provides a safe, sparse, and reproducible formula-to-matrix
pipeline for high-dimensional data. It extends R’s base `model.matrix()`
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
# install.packages("devtools")
devtools::install_github("gykwon-glitch/mmatrix")
```

## Basic usage

This section shows a typical workflow:

1.  Build a sparse design matrix and encoding spec from training data.
2.  Inspect sparsity / rank / dropped columns.
3.  Apply the same schema to new data, including unseen factor levels.

### 1. Build a sparse design matrix and spec

``` r
library(mmatrix)

set.seed(1)

# Toy training data: turn some variables into factors

train <- within(mtcars, {
cyl      <- factor(cyl) # categorical
am       <- factor(am, labels = c("auto", "manual"))
high_hp  <- factor(ifelse(hp > 120, "high", "low"))
})

# Construct sparse design matrix + encoding spec

fit <- mmatrix(
~ cyl + am + high_hp + wt + hp,
data        = train,
collin_tol  = 1e-9,
na_as_level = FALSE
)

# Pretty printing: uses print.mmatrix_spec()

print(fit)
#> mmatrix_spec object
#>   - original dims : 32 x 7
#>   - final design dims   : 32 x 7
#>   - rank (QR est., full / final): 7 / 7
#>   - nonzero (density) : 145 (0.6473)
#>   - dropped cols  : 0

# Basic summary: uses summary.mmatrix_spec()

summary(fit)
#> Summary of mmatrix_spec
#>   dims      : 32 x 7
#>   nonzero   : 145 (density = 0.6473)
#>   rank (full / final) : 7 / 7
#>   dropped   : 0 columns

# Under the hood, the sparse design matrix is in `fit$X`

dim(fit$X)
#> [1] 32  7
Matrix::nnzero(fit$X)
#> [1] 145
```

### 2. Inspect the design and factor encodings

You can get a compact report of the design matrix, collinearity drops,
and factor levels via `mm_report()`:

``` r
mm_report(fit)
#> 
#> ==== mm_report ====
#> Design (n x p): 32 x 7 | nnzero: 145 | density: 0.6473 | sparsity: 0.3527
#> Memory sparse: 0.0 MB | Dense-estimate: 0.0 MB
#> Estimated rank: 7
#> Dropped columns: 0
#> 
#> -- factor encodings (levels) --
#>  variable n_levels levels_preview
#>       cyl        3        4, 6, 8
#>        am        2   auto, manual
#>   high_hp        2      high, low
```

This prints:

- n x p, number of nonzeros, and density/sparsity,
- approximate memory usage for sparse vs dense representation,
- estimated rank (from QR),
- a head of drop_report (columns dropped because of all-zero or
  aliased),
- a preview of factor levels used in the encoding.

If you only want the returned object (without printing), you can capture
it:

``` r
rep_obj <- mm_report(fit)
#> 
#> ==== mm_report ====
#> Design (n x p): 32 x 7 | nnzero: 145 | density: 0.6473 | sparsity: 0.3527
#> Memory sparse: 0.0 MB | Dense-estimate: 0.0 MB
#> Estimated rank: 7
#> Dropped columns: 0
#> 
#> -- factor encodings (levels) --
#>  variable n_levels levels_preview
#>       cyl        3        4, 6, 8
#>        am        2   auto, manual
#>   high_hp        2      high, low
str(rep_obj, max.level = 1)
#> List of 9
#>  $ dims            : Named int [1:2] 32 7
#>   ..- attr(*, "names")= chr [1:2] "n" "p"
#>  $ nnzero          : int 145
#>  $ density         : num 0.647
#>  $ sparsity        : num 0.353
#>  $ mem_sparse_mb   : num 0.00595
#>  $ mem_dense_est_mb: num 0.00171
#>  $ rank            : int 7
#>  $ drop_report_head: NULL
#>  $ factor_levels   :'data.frame':    3 obs. of  3 variables:
```

### 2.1 Example: dropped columns from collinearity and all-zero

``` r
# Create a version of the training data with redundant columns
train_drop <- within(train, {
  hp_dup   <- hp   # perfectly collinear with hp
  all_zero <- 0    # all-zero column
})

fit_drop <- mmatrix(
  ~ cyl + am + high_hp + wt + hp + hp_dup + all_zero,
  data        = train_drop,
  collin_tol  = 1e-9,
  na_as_level = FALSE
)

mm_report(fit_drop)
#> 
#> ==== mm_report ====
#> Design (n x p): 32 x 7 | nnzero: 145 | density: 0.6473 | sparsity: 0.3527
#> Memory sparse: 0.0 MB | Dense-estimate: 0.0 MB
#> Estimated rank: 7
#> Dropped columns: 2
#> 
#> -- drop_report (head) --
#>    column     term          type             reason         metric protected
#>  all_zero all_zero         dummy           all-zero              0     FALSE
#>    hp_dup   hp_dup numeric/dummy aliased (rank-def) diagR<=1.0e-09     FALSE
#> 
#> -- factor encodings (levels) --
#>  variable n_levels levels_preview
#>       cyl        3        4, 6, 8
#>        am        2   auto, manual
#>   high_hp        2      high, low
```

### 3. Apply the schema to new data

To apply the same encoding (same columns & ordering) to new data, use
`mm_predict()`.

``` r
# New data with the same variables

newdata <- data.frame(
cyl      = factor(c(4, 6), levels = levels(train$cyl)),
am       = factor(c("manual", "auto"), levels = levels(train$am)),
high_hp  = factor(c("high", "low"), levels = levels(train$high_hp)),
wt       = c(2.2, 3.0),
hp       = c(150, 90)
)

# Apply the training schema to new data

X_new <- mm_predict(fit$spec, newdata = newdata, unknown = "zero")
X_new
#> 2 x 7 sparse Matrix of class "dgCMatrix"
#>   (Intercept) cyl6 cyl8 ammanual high_hplow  wt  hp
#> 1           1    .    .        1          . 2.2 150
#> 2           1    1    .        .          1 3.0  90
```

Here:

- `fit$spec` is the encoding specification created by `mmatrix()`
  (formula, terms object, levels map, column order, etc.).
- The returned `X_new` is a `Matrix::sparseMatrix` with the same columns
  and ordering as `fit$X`.

### 4. Handling unseen factor levels

`mm_predict()` lets you control what happens when newdata contains
levels that never appeared in the training data.

``` r
# Introduce an unseen level for 'cyl' and 'high_hp'

newdata2 <- data.frame(
cyl      = factor(c(4, 10), levels = c("4", "6", "8", "10")),
am       = factor(c("manual", "auto"), levels = levels(train$am)),
high_hp  = factor(c("high", "medium"), levels = c("high", "low", "medium")),
wt       = c(2.2, 3.0),
hp       = c(150, 90)
)

# Map unseen levels to the baseline (first training level)

# Under default treatment contrasts, this yields all-zero dummies.

X_zero <- mm_predict(fit$spec, newdata2, unknown = "zero")

dim(X_zero)
#> [1] 2 7
```

- `unknown = "zero"`: unseen levels are mapped to the baseline level
  (first training level), which typically yields all-zero dummy columns
  under default treatment contrasts.
- `unknown = "error"`: any unseen level causes an error, which can be
  useful in production pipelines where strict schema matching is
  required.
