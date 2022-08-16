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
  data <- left_join(cost[[1]], effect[[1]], by = c("id", "tx"))
  covariate_names <-
  data <- left_join(data, covariates, by = c("id", "tx"))
  model <- lm(nb_value ~ tx + covariates, data = data)

}
