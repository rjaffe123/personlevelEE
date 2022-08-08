#' Run ICER model
#'
#' @param cost result of [define_cost()]
#' @param effect result of [define_effect()]
#' @param covariates a dataframe of covariates as defined by [define_covariates()]
#' @param clustering clustering variable, default = NA
#' @param method type of model ("SLS", "hierarchical")
#' @param interaction interaction terms
#'
#' @return
#' @export
#'
#' @examples
run_icer_model <- function(cost, effect, covariates = NA, clustering = NA, interaction, method){
  ## if cluster == NA and model == "heirarchical" --> error
  ## build model based on method
  ## if SLS:
  # lm()
    # add interaction terms based on input

  ## if hierarchical:
  # lmer()
    # add interaction terms based on input

}
