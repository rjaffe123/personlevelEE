#' Run Incremental Net Benefit Model
#'
#' @param covariates a [define_covariates()] object
#' @param nb_value a [define_nb()] object
#'
#' @return
#' @export
#'
#' @examples
run_INB_model <- function(nb_value, covariates = NA) {
  # data for the model
  model <- lm(nb_value ~tx, data = data)

}
