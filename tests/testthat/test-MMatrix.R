#devtools::load_all()
library(testthat)
library(mmatrix)

test_that("prefer_keep_hard never drops protected columns", {
  set.seed(1)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1          # collinear
  x3 <- rnorm(n)
  df2 <- data.frame(y = rnorm(n), x1 = x1, x2 = x2, x3 = x3)
  form2 <- y ~ x1 + x2 + x3

  # 1) keep x1
  spec_pk1 <- mmatrix(
    form2, df2,
    prefer_keep      = "x1",
    prefer_keep_hard = TRUE,
    collin_tol       = 1e-9
  )

  expect_true("x1" %in% colnames(spec_pk1$X))
  expect_false("x1" %in% spec_pk1$drop_report$column)

  # 2) keep x2
  spec_pk2 <- mmatrix(
    form2, df2,
    prefer_keep      = "x2",
    prefer_keep_hard = TRUE,
    collin_tol       = 1e-9
  )

  expect_true("x2" %in% colnames(spec_pk2$X))
  expect_false("x2" %in% spec_pk2$drop_report$column)
})

test_that("soft prefer_keep is marked as protected if dropped", {
  set.seed(1)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1          # collinear
  x3 <- rnorm(n)
  df2 <- data.frame(y = rnorm(n), x1 = x1, x2 = x2, x3 = x3)
  form2 <- y ~ x1 + x2 + x3

  spec_soft <- mmatrix(form2, df2, prefer_keep = "x1", collin_tol = 1e-9)
  dr <- spec_soft$drop_report

  # if x1 is not dropped
  skip_if(!("x1" %in% dr$column), "x1 was not dropped under soft settings")

  # if x1 is dropped, protected=TRUE
  expect_true(dr$protected[dr$column == "x1"])
})

