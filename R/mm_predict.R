#' Schema-safe transform for new data
#' @param spec spec from mmatrix()
#' @param newdata data.frame
#' @param unknown c("other","zero","error")
#' @return sparseMatrix with the same columns/order as training
#' @export
mm_predict <- function(spec, newdata, unknown = c("other","zero","error")) {
  # TODO
}
