#' Run ICER model
#'
#' @param cost result of [define_cost()]
#' @param effect result of [define_effect()]
#' @param covariates a dataframe of covariates as defined by [define_covariates()]
#' @param clustering clustering variable, default = NA
#' @param method type of model ("SLS", "hierarchical", )
#' @param interaction interaction terms
#'
#' @return
#' @export
#'
#' @examples
run_icer_model <- function(cost, effect, covariates = NA, clustering = NA, method, interaction){
  ## if cluster == NA and model == "heirarchical" --> error
}
