#' Schema-safe sparse model matrix with QR-based collinearity handling
#'
#' Builds a sparse design matrix via \code{Matrix::sparse.model.matrix},
#' and removes aliased columns using a numerically stabilized QR decomposition.
#'
#' @param formula A model formula.
#' @param data A (cleaned) model.frame or data.frame.
#' @param collin_tol Numeric tolerance for \eqn{R} diagonals in the QR
#'   decomposition (for rank-deficiency detection).
#' @param prefer_keep Character vector of column names to prioritize during
#'   rank-deficiency resolution. These columns receive higher priority but
#'   may still be dropped unless \code{prefer_keep_hard = TRUE}.
#' @param prefer_keep_hard Logical; if \code{TRUE}, columns listed in
#'   \code{prefer_keep} are never dropped during the collinearity resolution
#'   step. Note that this may leave the final design matrix rank-deficient
#'   if collinearity involves only the hard-protected columns.
#' @param contrasts.arg Passed to \code{sparse.model.matrix}.
#' @param xlev Passed to \code{sparse.model.matrix}.
#' @param na_as_level Logical; treat NA values as their own factor level.
#' @param other_level A level to which unseen factor levels will be mapped
#'   (default: "Other").
#'
#' @return An object of class \code{mmatrix_spec}
#' @export
mmatrix <- function(formula, data, collin_tol = 1e-9,
                    prefer_keep = NULL,
                    prefer_keep_hard = FALSE,
                    contrasts.arg = NULL, xlev = NULL,
                    na_as_level = FALSE, other_level = "Other") {

  stopifnot(is.data.frame(data))

  # 1) Build the raw sparse model matrix
  X0 <- Matrix::sparse.model.matrix(
    object = formula,
    data = data,
    contrasts.arg = contrasts.arg,
    xlev = xlev
  )

  trm <- stats::terms(formula, data = data)
  assign_vec <- attr(X0, "assign")
  coln <- colnames(X0)
  term_labels <- c("(Intercept)", attr(trm, "term.labels"))

  # 2) Drop all-zero columns
  nz <- Matrix::colSums(X0 != 0)
  keep_idx <- which(nz > 0)
  drop_idx <- setdiff(seq_len(ncol(X0)), keep_idx)

  dr <- NULL
  if (length(drop_idx)) {
    dr <- data.frame(
      column    = coln[drop_idx],
      term      = term_labels[assign_vec[drop_idx] + 1L],
      type      = "dummy",
      reason    = "all-zero",
      metric    = 0,
      protected = FALSE,
      stringsAsFactors = FALSE
    )
  }

  X1 <- X0[, keep_idx, drop = FALSE]
  assign_vec1 <- assign_vec[keep_idx]
  coln1 <- colnames(X1)

  # 3) Column normalization + QR for rank estimation
  cnorm_full <- sqrt(Matrix::colSums(X1^2))
  cnorm_full[cnorm_full == 0] <- 1
  X1_qr_full <- X1 %*% Matrix::Diagonal(x = 1 / cnorm_full)

  QR_full <- Matrix::qr(X1_qr_full)
  R_full  <- Matrix::qr.R(QR_full)
  dR_full <- diag_abs_safe(R_full)

  rank_hat <- if (length(dR_full)) sum(dR_full > collin_tol) else 0L

  # 4) Collinearity handling (excluding hard-protected columns)
  #    - Hard-protected columns are never candidates for dropping.
  #    - If collinearity occurs solely among hard-protected columns,
  #      we keep them and allow the final matrix to remain rank-deficient.

  hard_mask <- rep(FALSE, length(coln1))
  if (!is.null(prefer_keep) && isTRUE(prefer_keep_hard)) {
    hard_mask <- coln1 %in% prefer_keep
  }

  # If all columns are hard-protected or no droppable columns exist
  if (all(hard_mask) || !any(!hard_mask)) {
    X2          <- X1
    assign_vec2 <- assign_vec1
  } else {

    sub_idx <- which(!hard_mask)
    X_sub   <- X1[, sub_idx, drop = FALSE]

    # QR on the droppable subset
    cnorm_sub <- sqrt(Matrix::colSums(X_sub^2))
    cnorm_sub[cnorm_sub == 0] <- 1
    X_sub_qr <- X_sub %*% Matrix::Diagonal(x = 1 / cnorm_sub)

    QR_sub <- Matrix::qr(X_sub_qr)
    R_sub  <- Matrix::qr.R(QR_sub)
    dR_sub <- diag_abs_safe(R_sub)

    p_sub   <- length(dR_sub)
    rank_sub <- if (p_sub) sum(dR_sub > collin_tol) else 0L
    piv_sub  <- QR_sub@q + 1L

    if (p_sub > 0L && rank_sub < p_sub) {
      # Local aliased columns within this subset
      aliased_local <- piv_sub[(rank_sub + 1L):p_sub]
      cand_idx      <- sort(sub_idx[aliased_local])

      # Soft protection (non-hard-protected columns only)
      prot_soft <- if (is.null(prefer_keep)) {
        rep(FALSE, length(cand_idx))
      } else {
        coln1[cand_idx] %in% prefer_keep
      }

      is_inter   <- grepl(":", coln1[cand_idx], fixed = TRUE)
      rare_score <- Matrix::colSums(X1[, cand_idx, drop = FALSE] != 0)

      # Priority order: keep soft-protected, interactions, and common levels
      ord   <- order(!prot_soft, !is_inter, rare_score)
      drop2 <- cand_idx[ord]

      # Soft-protected columns may still be dropped; record 'protected=TRUE'
      if (length(drop2)) {

        keep_mask     <- cand_idx %in% drop2
        is_inter_drop <- is_inter[keep_mask]
        prot_drop     <- prot_soft[keep_mask]

        dr2 <- data.frame(
          column    = coln1[drop2],
          term      = term_labels[assign_vec1[drop2] + 1L],
          type      = ifelse(is_inter_drop, "interaction", "numeric/dummy"),
          reason    = "aliased (rank-def)",
          metric    = sprintf("diagR<=%.1e", collin_tol),
          protected = prot_drop,
          stringsAsFactors = FALSE
        )

        dr <- rbind(dr, dr2)

        keep2       <- setdiff(seq_len(ncol(X1)), drop2)
        X2          <- X1[, keep2, drop = FALSE]
        assign_vec2 <- assign_vec1[keep2]

      } else {
        # Fallback: no drops (rare in practice)
        X2          <- X1
        assign_vec2 <- assign_vec1
      }

    } else {
      # No rank deficiency in the droppable subset
      X2          <- X1
      assign_vec2 <- assign_vec1
    }
  }

  # 5) Build schema specification object
  sp <- build_spec(
    formula     = formula,
    data        = data,
    terms_obj   = trm,
    levels_map  = get_levels_map(data, trm),
    contrasts   = contrasts.arg,
    other_level = other_level,
    na_as_level = na_as_level,
    colnames    = colnames(X2),
    assign_map  = assign_vec2
  )

  # 6) Create output object (S3)
  result <- structure(
    list(
      X           = X2,
      spec        = sp,
      drop_report = dr,
      rank        = rank_hat,    # estimated rank from full normalized QR
      assign_map  = assign_vec2,
      contrasts   = contrasts.arg,
      xlev        = xlev,
      orig_p      = ncol(X0),    # number of columns before dropping
      final_p     = ncol(X2)     # number of columns after dropping
    ),
    class = c("mmatrix_spec")
  )

  return(result)
}
