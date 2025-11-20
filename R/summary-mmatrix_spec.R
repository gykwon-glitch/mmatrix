#' Summary for mmatrix_spec objects
#'
#' @param object An object of class \code{mmatrix_spec}.
#' @param ... Ignored.
#'
#' @return A list with basic summary info (invisible).
#' @export
summary.mmatrix_spec <- function(object, ...) {
  stopifnot(inherits(object, "mmatrix_spec"))

  n  <- nrow(object$X)
  p  <- ncol(object$X)
  nnz <- Matrix::nnzero(object$X)
  dens <- nnz / (n * p)

  out <- list(
    n = n,
    p = p,
    nnzero = nnz,
    density = dens,
    rank = object$rank,
    dropped = object$drop_report
  )

  class(out) <- c("summary.mmatrix_spec", "list")

  # simple summary
  cat("Summary of mmatrix_spec\n")
  cat(sprintf("  dims     : %d x %d\n", n, p))
  cat(sprintf("  nonzero   : %d (density = %.4f)\n", nnz, dens))
  cat(sprintf("  rank     : %s\n",
              ifelse(is.null(object$rank), "NA", as.character(object$rank))))
  cat(sprintf("  dropped  : %d columns\n",
              ifelse(is.null(object$drop_report),
                     0L, nrow(object$drop_report %||% data.frame()))))

  invisible(out)
}
