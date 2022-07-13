#' Run ICER model
#'
#' @param cost
#' @param effect
#' @param covariates
#' @param clustering clustering variable, default = NA
#' @param method type of model ("SLS", "hierarchical", )
#' @param interaction interaction terms
#'
#' @return
#' @export
#'
#' @examples
run_icer_model <- function(cost, effect, covariates, clustering, method, interaction){
  ## if cluster == NA and model == "heirarchical" --> error
}
