#' Schema-safe sparse model matrix with QR-based collinearity handling
#'
#' Builds a sparse design matrix via \code{Matrix::sparse.model.matrix},
#' drops aliased columns using a numerically stabilized QR.
#'
#' @param formula A model formula.
#' @param data A (cleaned) model.frame or data.frame.
#' @param collin_tol Numeric tolerance for \eqn{R} diagonal (rank-def detection).
#' @param prefer_keep Character vector of column names to prioritize when
#'   resolving rank-deficiency. Columns in \code{prefer_keep} are dropped
#'   later than others, but may still be removed unless
#'   \code{prefer_keep_hard = TRUE}.
#' @param prefer_keep_hard Logical; if \code{TRUE}, columns listed in
#'   \code{prefer_keep} are never dropped as aliased, even if this leaves the
#'   design matrix rank-deficient (use with care).
#' @param contrasts.arg Passed to \code{sparse.model.matrix}.
#' @param xlev Passed to \code{sparse.model.matrix}.
#' @param na_as_level Logical; treat NA as its own factor level.
#' @param other_level Level to map unseen levels to ("Other").
#' @return An object of class \code{mmatrix_spec}
#' @export
mmatrix <- function(formula, data, collin_tol = 1e-9,
                    prefer_keep = NULL,
                    prefer_keep_hard = FALSE,
                    contrasts.arg = NULL, xlev = NULL,
                    na_as_level = FALSE, other_level = "Other") {

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

  # 3) normalization for QR
  cnorm <- sqrt(Matrix::colSums(X1^2))
  cnorm[cnorm == 0] <- 1
  X1_qr <- X1 %*% Matrix::Diagonal(x = 1 / cnorm)

  QR <- Matrix::qr(X1_qr)
  R  <- Matrix::qr.R(QR)
  dR <- diag_abs_safe(R)

  rank_hat <- if (length(dR)) sum(dR > collin_tol) else 0L
  piv <- QR@q + 1L

  aliased_local <- if (length(piv) > 0L && rank_hat < length(piv)) {
    piv[(rank_hat + 1L):length(piv)]
  } else {
    integer(0L)
  }

  # 4) aliased columns handling
  if (length(aliased_local)) {

    cand_idx <- sort(aliased_local)

    prot <- if (is.null(prefer_keep)) {
      rep(FALSE, length(cand_idx))
    } else {
      coln1[cand_idx] %in% prefer_keep
    }

    is_inter   <- grepl(":", coln1[cand_idx], fixed = TRUE)
    rare_score <- Matrix::colSums(X1[, cand_idx, drop = FALSE] != 0)

    ord   <- order(!prot, !is_inter, rare_score)
    drop2 <- cand_idx[ord]

    # HARD POLICY: never drop prefer_keep columns
    if (isTRUE(prefer_keep_hard) && !is.null(prefer_keep)) {
      hard_keep_idx <- cand_idx[prot]   # == cand_idx[coln1[cand_idx] %in% prefer_keep]
      drop2 <- setdiff(drop2, hard_keep_idx)
    }

    if (length(drop2)) {

      keep_mask <- cand_idx %in% drop2
      is_inter_drop <- is_inter[keep_mask]
      prot_drop <- prot[keep_mask]

      dr2 <- data.frame(
        column   = coln1[drop2],
        term     = term_labels[assign_vec1[drop2] + 1L],
        type     = ifelse(is_inter_drop, "interaction", "numeric/dummy"),
        reason   = "aliased (rank-def)",
        metric   = sprintf("diagR<=%.1e", collin_tol),
        protected = prot_drop,
        stringsAsFactors = FALSE
      )

      dr <- rbind(dr, dr2)

      keep2       <- setdiff(seq_len(ncol(X1)), drop2)
      X2          <- X1[, keep2, drop = FALSE]
      assign_vec2 <- assign_vec1[keep2]

    } else {
      X2          <- X1
      assign_vec2 <- assign_vec1
    }

  } else {
    X2          <- X1
    assign_vec2 <- assign_vec1
  }

  # 5) build schema spec
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

  # 6) create object (S3)
  result <- structure(
    list(
      X           = X2,
      spec        = sp,
      drop_report = dr,
      rank        = rank_hat,
      assign_map  = assign_vec2,
      contrasts   = contrasts.arg,
      xlev        = xlev,
      orig_p      = ncol(X0),     # original dimension p
      final_p     = ncol(X2)      # final dimension p
    ),
    class = c("mmatrix_spec")
  )

  return(result)
}
