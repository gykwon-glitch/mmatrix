#' Summary for mmatrix_spec objects
#'
#' @param object An object of class \code{mmatrix_spec}.
#' @param ... Ignored.
#'
#' @return A list with basic summary info (invisible).
#' @export
summary.mmatrix_spec <- function(object, ...) {
  stopifnot(inherits(object, "mmatrix_spec"))

  X <- object$X
  n <- nrow(X)
  p <- ncol(X)
  nnz <- Matrix::nnzero(X)
  dens <- if (n * p > 0) nnz / (n * p) else NA_real_

  rank_full  <- if (!is.null(object$rank_full))  object$rank_full  else object$rank
  rank_final <- if (!is.null(object$rank_final)) object$rank_final else object$rank
  drop_n     <- if (is.null(object$drop_report)) 0L else nrow(object$drop_report)

  cat("Summary of mmatrix_spec\n")
  cat(sprintf("  dims      : %d x %d\n", n, p))
  cat(sprintf("  nonzero   : %d (density = %.4f)\n", nnz, dens))
  cat(sprintf("  rank (full / final) : %d / %d\n", rank_full, rank_final))
  cat(sprintf("  dropped   : %d columns\n", drop_n))

  if (!is.null(rank_final) && rank_final < p) {
    cat(sprintf("  * Note: final design still rank-deficient (rank %d < %d cols) under tol.\n",
                rank_final, p))
  }

  invisible(object)
}
