#' Schema-safe sparse model matrix with QR-based collinearity handling
#'
#' Builds a sparse design matrix via \code{Matrix::sparse.model.matrix},
#' drops aliased columns using a numerically stabilized QR.
#'
#' @param formula A model formula.
#' @param data A (cleaned) model.frame or data.frame.
#' @param collin_tol Numeric tolerance for \eqn{R} diagonal (rank-def detection).
#' @param prefer_keep Character vector of column names to prioritize when
#'   resolving rank-deficiency. Columns in \code{prefer_keep} are annotated
#'   and given higher priority, but may still be removed unless
#'   \code{prefer_keep_hard = TRUE}.
#' @param prefer_keep_hard Logical; if \code{TRUE}, columns listed in
#'   \code{prefer_keep} are never considered as drop candidates in the
#'   collinearity resolution step. This may leave the final design matrix
#'   rank-deficient if collinearity only involves hard-protected columns.
#' @param contrasts.arg Passed to \code{sparse.model.matrix}.
#' @param xlev Passed to \code{sparse.model.matrix}.
#' @param na_as_level Logical; treat NA as its own factor level.
#'
#' @return An object of class \code{mmatrix_spec}
#' @export
mmatrix <- function(formula, data, collin_tol = 1e-9,
                    prefer_keep = NULL,
                    prefer_keep_hard = FALSE,
                    contrasts.arg = NULL, xlev = NULL,
                    na_as_level = FALSE) {

  stopifnot(is.data.frame(data))

  # 1) raw sparse model matrix
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

  # 2) drop all-zero columns
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

  # 3) Full-matrix normalization + QR (for rank estimation and alias detection)
  cnorm_full <- sqrt(Matrix::colSums(X1^2))
  cnorm_full[cnorm_full == 0] <- 1
  X1_qr_full <- X1 %*% Matrix::Diagonal(x = 1 / cnorm_full)

  QR_full <- Matrix::qr(X1_qr_full)
  R_full  <- Matrix::qr.R(QR_full)
  dR_full <- diag_abs_safe(R_full)

  p_full    <- length(dR_full)
  rank_full <- if (p_full) sum(dR_full > collin_tol) else 0L
  piv_full  <- QR_full@q + 1L

  # 4) Handling aliased columns
  #    (Candidates are identified from the full X1; hard/soft prefer_keep
  #     rules are applied only when choosing which candidates to drop.)
  if (p_full > 0L && rank_full < p_full) {
    # Aliased columns in the full matrix (indices in X1)
    aliased_local <- piv_full[(rank_full + 1L):p_full]
    cand_idx      <- sort(aliased_local)

    # Soft prefer_keep flags (based on full X1)
    prot_soft <- if (is.null(prefer_keep)) {
      rep(FALSE, length(cand_idx))
    } else {
      coln1[cand_idx] %in% prefer_keep
    }

    is_inter   <- grepl(":", coln1[cand_idx], fixed = TRUE)
    rare_score <- Matrix::colSums(X1[, cand_idx, drop = FALSE] != 0)

    # Hard-protected columns: removed only from the drop-candidate set
    hard_mask_cand <- rep(FALSE, length(cand_idx))
    if (!is.null(prefer_keep) && isTRUE(prefer_keep_hard)) {
      hard_mask_cand <- coln1[cand_idx] %in% prefer_keep
    }

    cand_for_drop_idx <- cand_idx[!hard_mask_cand]
    prot_for_drop     <- prot_soft[!hard_mask_cand]
    is_inter_dropset  <- is_inter[!hard_mask_cand]
    rare_dropset      <- rare_score[!hard_mask_cand]

    if (length(cand_for_drop_idx)) {

      # Drop ordering: soft prefer_keep is a weak priority
      ord   <- order(!prot_for_drop, !is_inter_dropset, rare_dropset)
      drop2 <- cand_for_drop_idx[ord]

      # Match protected/interaction attributes using original cand_idx positions
      pos_in_cand   <- match(drop2, cand_idx)
      is_inter_drop <- is_inter[pos_in_cand]
      prot_drop     <- prot_soft[pos_in_cand]

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
      # If no aliased columns can be dropped due to hard-protection:
      # allow the final design matrix to remain rank-deficient
      X2          <- X1
      assign_vec2 <- assign_vec1
    }

  } else {
    # No rank deficiency → nothing to drop
    X2          <- X1
    assign_vec2 <- assign_vec1
  }

  # 5) Recompute rank using the final design matrix (rank_final)
  cnorm_fin <- sqrt(Matrix::colSums(X2^2))
  cnorm_fin[cnorm_fin == 0] <- 1
  X2_qr <- X2 %*% Matrix::Diagonal(x = 1 / cnorm_fin)

  QR_fin <- Matrix::qr(X2_qr)
  R_fin  <- Matrix::qr.R(QR_fin)
  dR_fin <- diag_abs_safe(R_fin)
  rank_final <- if (length(dR_fin)) sum(dR_fin > collin_tol) else 0L

  # 6) Build schema spec
  sp <- build_spec(
    formula     = formula,
    data        = data,
    terms_obj   = trm,
    levels_map  = get_levels_map(data, trm),
    contrasts   = contrasts.arg,
    na_as_level = na_as_level,
    colnames    = colnames(X2),
    assign_map  = assign_vec2
  )

  # 7) Create object (S3)
  result <- structure(
    list(
      X           = X2,
      spec        = sp,
      drop_report = dr,
      rank_full   = rank_full,     # Rank based on full X1
      rank_final  = rank_final,    # Rank based on final X2
      rank        = rank_final,    # backward-compatible 'rank' field
      assign_map  = assign_vec2,
      contrasts   = contrasts.arg,
      xlev        = xlev,
      orig_p      = ncol(X0),      # Original number of columns
      final_p     = ncol(X2)       # Final number of columns
    ),
    class = c("mmatrix_spec")
  )

  return(result)
}
