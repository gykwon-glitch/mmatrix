#' Summary report for an mmatrix spec
#'
#' Produces a compact summary of an `mmatrix` specification, including
#' column roles, types, factor levels, and memory usage of the design matrix.
#'
#' @param spec A specification object produced by `mmatrix()`.
#' @param show_levels Logical; if `TRUE`, include a preview of factor levels
#'   for each categorical column. Default is `TRUE`.
#' @param max_levels_preview Integer; maximum number of levels to show per
#'   factor when `show_levels = TRUE`. Default is `10`.
#'
#' @return A named list summarizing the encoded design matrix and its encodings,
#'   with the following components:
#'
#'   \describe{
#'     \item{dims}{Named integer vector with the dimensions of the design
#'       matrix, \code{c(n, p)}, where \code{n} is the number of rows and
#'       \code{p} is the number of columns.}
#'
#'     \item{nnzero}{Total number of non-zero entries in the sparse design
#'       matrix (as computed by \code{Matrix::nnzero()}).}
#'
#'     \item{density}{Proportion of non-zero entries in the design matrix:
#'       \code{nnzero / (n * p)}.}
#'
#'     \item{sparsity}{Proportion of zero entries in the design matrix:
#'       \code{1 - density}.}
#'
#'     \item{mem_sparse_mb}{Approximate memory usage (in megabytes) of the
#'       sparse design matrix object (based on \code{utils::object.size()}).}
#'
#'     \item{mem_dense_est_mb}{Approximate memory usage (in megabytes) that a
#'       dense \code{n x p} \code{double} matrix would require
#'       (\code{n * p * 8} bytes).}
#'
#'     \item{rank}{Estimated numerical rank of the design matrix, typically
#'       carried over from \code{spec$rank}.}
#'
#'     \item{drop_report_head}{Either \code{NULL} or a small
#'       \code{data.frame} containing the first few rows (up to 10) of the
#'       drop report, summarizing columns that were dropped due to zero
#'       variance, perfect collinearity, or other preprocessing.}
#'
#'     \item{factor_levels}{Either \code{NULL} or a \code{data.frame} that
#'       summarizes factor encodings used in the design. Typical columns are:
#'       \describe{
#'         \item{variable}{Name of the original factor variable.}
#'         \item{n_levels}{Number of levels of the factor.}
#'         \item{levels_preview}{Comma-separated preview of the first
#'           \code{max_levels_preview} levels, as shown in the printed report.}
#'       }}
#'   }
#'
#' @importFrom utils object.size
#' @export
#'
#' @examples
#' \dontrun{
#'   spec <- mmatrix(~ Species + Sepal.Length, iris)
#'   mm_report(spec)
#' }
#'#' @importFrom utils object.size

mm_report <- function(spec,
                      show_levels = TRUE,
                      max_levels_preview = 6) {
  stopifnot(inherits(spec$X, "sparseMatrix"))

  n  <- nrow(spec$X); p <- ncol(spec$X)
  nnz <- Matrix::nnzero(spec$X)
  dens <- nnz / (n * p)
  mem_sparse <- as.numeric(object.size(spec$X)) / 1024^2
  mem_dense_est <- as.numeric(n * p * 8) / 1024^2

  dr <- spec$drop_report
  drop_count <- if (is.null(dr)) 0L else nrow(dr)
  drop_head  <- if (!is.null(dr) && nrow(dr)) utils::head(dr, 10) else NULL

  levels_tbl <- NULL
  if (isTRUE(show_levels) && !is.null(spec$spec$levels_map)) {
    levels_tbl <- do.call(
      rbind,
      lapply(names(spec$spec$levels_map), function(v) {
        lv <- spec$spec$levels_map[[v]]
        data.frame(
          variable = v,
          n_levels = length(lv),
          levels_preview = paste(
            utils::head(lv, max_levels_preview),
            collapse = ", "
          ),
          stringsAsFactors = FALSE
        )
      })
    )
  }

  cat("\n==== mm_report ====\n")
  cat(sprintf(
    "Design (n x p): %d x %d | nnzero: %d | density: %.4f | sparsity: %.4f\n",
    n, p, nnz, dens, 1 - dens
  ))
  cat(sprintf(
    "Memory sparse: %.1f MB | Dense-estimate: %.1f MB\n",
    mem_sparse, mem_dense_est
  ))
  cat(sprintf(
    "Estimated rank: %s\n",
    ifelse(is.null(spec$rank), "NA", as.character(spec$rank))
  ))
  cat(sprintf("Dropped columns: %d\n", drop_count))

  if (!is.null(drop_head)) {
    cat("\n-- drop_report (head) --\n")
    print(drop_head, row.names = FALSE)
  }
  if (!is.null(levels_tbl)) {
    cat("\n-- factor encodings (levels) --\n")
    print(levels_tbl, row.names = FALSE)
  }

  invisible(list(
    dims = c(n = n, p = p),
    nnzero = nnz,
    density = dens,
    sparsity = 1 - dens,
    mem_sparse_mb = mem_sparse,
    mem_dense_est_mb = mem_dense_est,
    rank = spec$rank,
    drop_report_head = drop_head,
    factor_levels = levels_tbl
  ))
}
