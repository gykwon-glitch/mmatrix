#' Pretty printing for mmatrix_spec objects
#'
#' @param x An object of class \code{mmatrix_spec}.
#' @param ... Ignored (for compatibility with generic).
#'
#' @export
print.mmatrix_spec <- function(x, ...) {
  stopifnot(inherits(x, "mmatrix_spec"))

  X <- x$X
  n <- nrow(X)
  p <- ncol(X)
  nnz <- Matrix::nnzero(X)
  dens <- if (n * p > 0) nnz / (n * p) else NA_real_

  rank_full  <- if (!is.null(x$rank_full))  x$rank_full  else x$rank
  rank_final <- if (!is.null(x$rank_final)) x$rank_final else x$rank
  drop_n     <- if (is.null(x$drop_report)) 0L else nrow(x$drop_report)

  cat("mmatrix_spec object\n")
  cat(sprintf("  - original dims : %d x %d\n", n, x$orig_p))
  cat(sprintf("  - final design dims   : %d x %d\n", n, p))
  cat(sprintf("  - rank (QR est., full / final): %d / %d\n",
              rank_full, rank_final))
  cat(sprintf("  - nonzero (density) : %d (%.4f)\n", nnz, dens))
  cat(sprintf("  - dropped cols  : %d", drop_n))

  if (drop_n > 0L) {
    show_n <- min(drop_n, 5L)
    cat(sprintf(" (showing up to %d)\n", show_n))
    print(utils::head(x$drop_report, show_n), row.names = FALSE)
  } else {
    cat("\n")
  }

  # warning rank-def
  if (!is.null(rank_final) && rank_final < p) {
    cat(sprintf("  * Note: final design still rank-deficient (rank %d < %d cols) under tol.\n",
                rank_final, p))
  }

  invisible(x)
}
