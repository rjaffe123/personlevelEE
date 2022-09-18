#' Define Covariates
#'
#' Takes a list of covariates and stores them for future use.
#'
#' @param names list of names of the covariates
#' @param id vector of numerical ID's
#' @param covariates a list of vectors corresponding to the covariates
#'
#' @return a dataframe of covariates linked to numerical ID's
#' @export
#'
#' @examples
define_covariates <- function(covariates, names, id){
  covariates <- as.data.frame(do.call(cbind, covariates))
  colnames(covariates) <- names
  covariates <- cbind(id, covariates)
  structure(
    list(data_covariates = covariates
    ),
    class("define_covariates")
  )
}

#' Print Covariates
#'
#' @param x an [define_covariates()] object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.define_covariates <- function(x, ...){
    print(head(x$data_covariates))
}
