#' Pretty printing for mmatrix_spec objects
#'
#' @param x An object of class \code{mmatrix_spec}.
#' @param ... Ignored (for compatibility with generic).
#'
#' @export
print.mmatrix_spec <- function(x, ...) {
  stopifnot(inherits(x, "mmatrix_spec"))

  n  <- nrow(x$X)
  p  <- ncol(x$X)
  nnz <- Matrix::nnzero(x$X)
  dens <- nnz / (n * p)

  cat("mmatrix_spec object\n")
  cat(sprintf("  - design dims   : %d x %d\n", n, p))
  cat(sprintf("  - rank (QR est.): %s\n",
              ifelse(is.null(x$rank), "NA", as.character(x$rank))))
  cat(sprintf("  - nonzero (density) : %d (%.4f)\n", nnz, dens))

  if (!is.null(x$drop_report) && nrow(x$drop_report) > 0) {
    cat(sprintf("  - dropped cols  : %d (showing up to 5)\n",
                nrow(x$drop_report)))
    print(utils::head(x$drop_report, 5), row.names = FALSE)
  } else {
    cat("  - dropped cols  : 0\n")
  }

  invisible(x)
}
