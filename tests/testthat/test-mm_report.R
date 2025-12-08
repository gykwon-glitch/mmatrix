#devtools::load_all()
library(testthat)
library(mmatrix)

test_that("mm_report basic summary is computed correctly", {
  skip_if_not_installed("Matrix")

  set.seed(1L)
  X <- Matrix::Matrix(
    c(1, 0, 0,
      0, 2, 0,
      0, 0, 3,
      0, 4, 0),
    nrow = 4, ncol = 3, sparse = TRUE
  )
  # nnz = 4, n = 4, p = 3 → density = 4 / 12 = 1/3
  spec <- list(
    X           = X,
    rank        = 3L,
    drop_report = data.frame(
      column  = c("x3"),
      reason  = c("collinear"),
      stringsAsFactors = FALSE
    ),
    spec = list(
      levels_map = NULL
    )
  )

  out_capture <- capture.output(
    res <- mm_report(spec)
  )

  # Return value should be an invisible list
  expect_type(res, "list")
  expect_equal(res$dims, c(n = 4L, p = 3L))
  expect_equal(res$nnzero, 4L)
  expect_equal(res$density, 4 / 12)
  expect_equal(res$sparsity, 1 - 4 / 12)
  expect_equal(res$rank, 3L)

  # drop_report_head should contain exactly one row
  expect_s3_class(res$drop_report_head, "data.frame")
  expect_equal(nrow(res$drop_report_head), 1L)

  # Console output should contain core summary information
  expect_true(any(grepl("Design \\(n x p\\): 4 x 3", out_capture)))
  expect_true(any(grepl("Dropped columns: 1", out_capture)))
})

test_that("mm_report works when drop_report and levels_map are NULL", {
  skip_if_not_installed("Matrix")

  X <- Matrix::Diagonal(5L)  # 5 x 5 identity → nnz = 5
  spec <- list(
    X           = X,
    rank        = NULL,
    drop_report = NULL,
    spec        = list(
      levels_map = NULL
    )
  )

  out_capture <- capture.output(
    res <- mm_report(spec)
  )

  # When rank is NULL, printed value is 'NA', but returned value remains NULL
  expect_null(res$rank)
  expect_equal(res$drop_report_head, NULL)
  expect_equal(res$factor_levels, NULL)

  # Just check that the output prints without error and includes key lines
  expect_true(any(grepl("Design \\(n x p\\): 5 x 5", out_capture)))
  expect_true(any(grepl("Dropped columns: 0", out_capture)))
})

test_that("mm_report summarizes factor levels when levels_map is present", {
  skip_if_not_installed("Matrix")

  X <- Matrix::Diagonal(3L)
  levels_map <- list(
    color = c("red", "blue", "green"),
    group = c("A", "B")
  )

  spec <- list(
    X           = X,
    rank        = 3L,
    drop_report = NULL,
    spec        = list(
      levels_map = levels_map
    )
  )

  out_capture <- capture.output(
    res <- mm_report(spec)
  )

  # Check factor_levels summary table
  lf <- res$factor_levels
  expect_s3_class(lf, "data.frame")
  expect_equal(nrow(lf), 2L)
  expect_equal(sort(lf$variable), c("color", "group"))

  # Check number of levels per variable
  nlv <- setNames(lf$n_levels, lf$variable)
  expect_equal(nlv[["color"]], 3L)
  expect_equal(nlv[["group"]], 2L)

  # Console output should include the factor encodings section
  expect_true(any(grepl("factor encodings \\(levels\\)", out_capture)))
})

test_that("mm_report respects show_levels = FALSE", {
  skip_if_not_installed("Matrix")

  X <- Matrix::Diagonal(2L)
  levels_map <- list(
    f = c("a", "b", "c")
  )

  spec <- list(
    X           = X,
    rank        = 2L,
    drop_report = NULL,
    spec        = list(
      levels_map = levels_map
    )
  )

  out_capture <- capture.output(
    res <- mm_report(spec, show_levels = FALSE)
  )

  # With show_levels = FALSE, factor_levels summary should not be generated
  expect_null(res$factor_levels)
  expect_false(any(grepl("factor encodings \\(levels\\)", out_capture)))
})
