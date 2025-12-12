#devtools::load_all()
library(testthat)
library(mmatrix)
library(Matrix)

test_that("mm_predict basic spec construction helper works", {
  # Just sanity: helper to build a minimal spec from training data
  train <- data.frame(
    color = factor(c("red", "blue", "red"), levels = c("red", "blue")),
    x     = c(1, 2, 3)
  )
  fml <- ~ color + x

  X_train <- Matrix::sparse.model.matrix(fml, data = train)

  spec <- list(
    terms       = stats::terms(fml),
    formula     = fml,
    levels_map  = list(color = levels(train$color)),
    col_order   = colnames(X_train),
    contrasts   = NULL,
    na_as_level = FALSE
  )

  expect_true(is.list(spec))
  expect_true(all(spec$col_order == colnames(X_train)))
})

test_that("mm_predict unknown = 'error' throws on unseen levels", {
  train <- data.frame(
    color = factor(c("red", "blue", "red"), levels = c("red", "blue")),
    x     = c(1, 2, 3)
  )
  fml <- ~ color + x
  X_train <- Matrix::sparse.model.matrix(fml, data = train)

  spec <- list(
    terms       = stats::terms(fml),
    formula     = fml,
    levels_map  = list(color = levels(train$color)),
    col_order   = colnames(X_train),
    contrasts   = NULL,
    na_as_level = FALSE
  )

  newdata <- data.frame(
    color = factor("green"),  # unseen level
    x     = 10
  )

  expect_error(
    mm_predict(spec, newdata, unknown = "error"),
    "Unseen level"
  )
})

test_that("mm_predict unknown = 'zero' maps unseen levels to baseline and keeps col_order", {
  train <- data.frame(
    color = factor(c("red", "blue", "red"), levels = c("red", "blue")),
    x     = c(1, 2, 3)
  )
  fml <- ~ color + x
  X_train <- Matrix::sparse.model.matrix(fml, data = train)

  spec <- list(
    terms       = stats::terms(fml),
    formula     = fml,
    levels_map  = list(color = levels(train$color)),  # no "other" level yet
    col_order   = colnames(X_train),
    contrasts   = NULL,
    na_as_level = FALSE
  )

  newdata <- data.frame(
    color = factor(c("green", "red")),  # one unseen, one seen
    x     = c(10, 20)
  )

  X_new <- mm_predict(spec, newdata, unknown = "zero")

  # identical colnames
  expect_identical(colnames(X_new), spec$col_order)

  # same row numbers: unseen levels do not drop rows
  expect_equal(nrow(X_new), nrow(newdata))
})

test_that("mm_predict pads missing columns with zeros", {
  train <- data.frame(
    color = factor(c("red", "blue", "red"), levels = c("red", "blue")),
    x     = c(1, 2, 3)
  )
  fml <- ~ color + x
  X_train <- Matrix::sparse.model.matrix(fml, data = train)

  # normal training colnames
  base_cols <- colnames(X_train)

  # add fake column : "want" > "have"
  fake_col <- "fake_dummy_col"
  spec <- list(
    terms       = stats::terms(fml),
    formula     = fml,
    levels_map  = list(color = levels(train$color)),
    col_order   = c(base_cols, fake_col),
    contrasts   = NULL,
    na_as_level = FALSE
  )

  # normal newdata
  newdata <- data.frame(
    color = factor("red", levels = c("red", "blue")),
    x     = 10
  )

  X_new <- mm_predict(spec, newdata, unknown = "zero")

  # fake_col should exist
  expect_true(fake_col %in% colnames(X_new))

  # fake_col should be all zero
  expect_true(all(as.numeric(X_new[, fake_col]) == 0))

  # same col_order
  expect_identical(colnames(X_new), spec$col_order)
})


test_that("mm_predict unknown = 'zero' maps unseen levels to all-zero dummies", {
  train <- data.frame(
    color = factor(c("red", "blue", "red"), levels = c("red", "blue")),
    x     = c(1, 2, 3)
  )
  fml <- ~ color + x
  X_train <- Matrix::sparse.model.matrix(fml, data = train)

  spec <- list(
    terms       = stats::terms(fml),
    formula     = fml,
    levels_map  = list(color = levels(train$color)),
    col_order   = colnames(X_train),
    contrasts   = NULL,
    na_as_level = FALSE
  )

  newdata <- data.frame(
    color = factor("green"),  # unseen level
    x     = 10
  )

  X_new <- mm_predict(spec, newdata, unknown = "zero")

  # Columns corresponding to color dummies
  color_cols <- grep("^color", colnames(X_new))
  expect_true(length(color_cols) >= 1)

  # For unseen level with unknown = 'zero',
  # all color dummy entries should be 0
  expect_true(all(as.numeric(X_new[1, color_cols]) == 0))

  # Column set/order should exactly match training
  expect_identical(colnames(X_new), spec$col_order)
})



