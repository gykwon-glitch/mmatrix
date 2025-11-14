# R/utils.R
# Internal helper functions for mmatrix
# These are not exported; used internally by mmatrix(), mm_predict(), etc.

#' @keywords internal
#' @noRd
#' @importFrom methods as
diag_abs_safe <- function(M) {
  # Safely extract absolute values of diagonal elements from a Matrix object,
  # even if conversion is needed (for dense fallback).
  result <- tryCatch({
    as.numeric(abs(Matrix::diag(M)))
  }, error = function(e) {
    as.numeric(abs(diag(as(M, "dgeMatrix"))))
  })
  return(result)
}

#' @keywords internal
#' @noRd
get_levels_map <- function(data, terms_obj) {
  # Extract factor levels for each variable in the formula terms
  v <- all.vars(terms_obj)
  levs <- lapply(v, function(nm) {
    if (!nm %in% names(data)) return(NULL)
    x <- data[[nm]]
    if (is.factor(x)) levels(x) else NULL
  })
  names(levs) <- v
  result <- Filter(Negate(is.null), levs)
  return(result)
}

#' @keywords internal
#' @noRd
build_spec <- function(formula, data, terms_obj, levels_map, contrasts,
                       other_level, na_as_level, colnames, assign_map) {
  # Package all schema info into a structured list
  spec_list <- list(
    formula      = formula,
    terms        = terms_obj,
    levels_map   = levels_map,
    contrasts    = contrasts,
    other_level  = other_level,
    na_as_level  = na_as_level,
    col_order    = colnames,
    assign_map   = assign_map
  )
  return(spec_list)
}

