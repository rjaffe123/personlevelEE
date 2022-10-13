#' Define Covariates
#'
#' Takes a list of covariates and stores them for future use.
#'
#' @param id vector of numerical ID's
#' @param covariates a list of vectors corresponding to the covariates
#' @param names names of the covariates in order of them listed
#'
#' @return a dataframe of covariates linked to numerical ID's
#' @export
#'
#' @example inst/examples/example_define_covariates.R
define_covariates <- function(covariates, names, id){
  covariate_df <- data.frame(cov = covariates[[1]])
  for (x in covariates){
    covariate_df <- cbind(covariate_df, x)
  }
  # covariates <- as.data.frame(do.call(cbind, covariates)) ## this changes a numeric to a character - NEED TO FIGURE OUT WHY
  covariate_df <- covariate_df[, -1]
  colnames(covariate_df) <- names
  covariate_df <- cbind(id, covariate_df)
  structure(
    list(data_covariates = covariate_df,
         names = names
    ),
    class= "define_covariates")
}

#' Print Covariates
#'
#' @param x an [define_covariates()] object
#' @param ...
#'
#' @export
#'
print.define_covariates <- function(x, ...){
    print(head(x$data_covariates))
}
