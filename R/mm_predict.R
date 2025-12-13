#' Schema-safe transform for new data
#'
#' Apply an `mmatrix()` spec to new data, returning a sparse design matrix
#' with the same columns and ordering as in training.
#'
#' @param spec Spec object produced by `mmatrix()`.
#' @param newdata A `data.frame` of new observations.
#' @param unknown How to handle unseen factor levels in `newdata`:
#'   one of `"zero"` or `"error"`.
#'   \itemize{
#'     \item \code{"zero"}: map unseen levels to the baseline level
#'       (first training level). Under default treatment contrasts,
#'       this yields all-zero dummy columns.
#'     \item \code{"error"}: throw an error if any unseen level is found.
#'   }
#'
#' @return A `Matrix::sparseMatrix` with the same columns/order as the
#'   training design matrix.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   spec <- mmatrix(~ color + x, data = df_train)
#'   X_new <- mm_predict(spec, df_new, unknown = "zero")
#' }
mm_predict <- function(spec, newdata, unknown = c("zero", "error")) {
  # Match unknown handling policy
  unknown <- match.arg(unknown)

  # Basic spec sanity checks
  stopifnot(
    is.list(spec),
    !is.null(spec$terms),
    !is.null(spec$formula),
    !is.null(spec$levels_map),
    !is.null(spec$col_order)
  )

  # Coerce factors/characters to training schema levels and apply unknown-level policy
  nd <- newdata

  for (nm in names(spec$levels_map)) {
    if (!nm %in% names(nd)) next

    x <- nd[[nm]]

    # Handle both factor and character inputs
    if (is.factor(x) || is.character(x)) {
      lv <- spec$levels_map[[nm]]

      # Work on character representation first
      xchar <- as.character(x)

      # Detect unseen levels relative to training levels
      unseen <- setdiff(unique(xchar), c(lv, NA))

      if (length(unseen)) {
        if (unknown == "error") {
          stop(sprintf("Unseen level in '%s': %s",
                       nm, paste(unseen, collapse = ", ")))
        }

        # unknown == "zero":
        # Map all unseen levels to the baseline level (first training level).
        # Under the default treatment contrasts, this gives all-zero dummies.
        baseline_level <- lv[1L]
        xchar[!(xchar %in% lv)] <- baseline_level

        x <- factor(
          xchar,
          levels  = lv,
          exclude = if (isTRUE(spec$na_as_level)) NULL else NA
        )

      } else {
        # No unseen levels: just coerce to the training levels
        x <- factor(
          xchar,
          levels  = lv,
          exclude = if (isTRUE(spec$na_as_level)) NULL else NA
        )
      }

      nd[[nm]] <- x
    }
  }

  # Build sparse design matrix using the training formula/contrasts
  Xn <- Matrix::sparse.model.matrix(
    object        = spec$formula,
    data          = nd,
    contrasts.arg = spec$contrasts
  )

  # Align columns to the training schema (same set + same order)
  want <- spec$col_order
  have <- colnames(Xn)

  # Drop extra columns not present in the training schema
  extra_cols <- setdiff(have, want)
  if (length(extra_cols)) {
    Xn <- Xn[, have %in% want, drop = FALSE]
    have <- colnames(Xn)
  }

  # Add missing columns as all-zero sparse columns
  missing_cols <- setdiff(want, have)
  if (length(missing_cols)) {
    pad <- Matrix::Matrix(
      0,
      nrow   = nrow(Xn),
      ncol   = length(missing_cols),
      sparse = TRUE
    )
    colnames(pad) <- missing_cols

    # Use base cbind() – dispatches to Matrix methods for sparse matrices
    Xn <- cbind(Xn, pad)
  }

  # Final column reordering to match the training schema exactly
  Xn <- Xn[, want, drop = FALSE]

  return(Xn)
}
