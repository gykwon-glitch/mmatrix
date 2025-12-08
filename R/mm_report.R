#' Schema-safe transform for new data
#'
#' Applies the encoding schema learned by `mmatrix()` to new data,
#' ensuring the same column set and ordering as in the training design matrix.
#'
#' @param spec A specification object produced by `mmatrix()`.
#' @param newdata A `data.frame` of new observations.
#' @param unknown How to handle unseen factor levels:
#'   `"other"` (map to `spec$other_level`),
#'   `"zero"` (map to baseline → zero dummy),
#'   `"error"` (stop with an error).
#'   Default: `c("other","zero","error")`.
#'
#' @return A sparse design matrix whose columns and ordering
#'   match those of the training matrix.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   spec <- mmatrix(~ color + x, iris)
#'   X_new <- mm_predict(spec, iris[1:5, ])
#' }
#'
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
