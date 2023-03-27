#' Define Covariates
#'
#' Takes a list of covariates and stores them for future use.
#'
#' @param data_frame a data frame that contains the following information
#' @param id name of ID column
#' @param covariate_names a list of the names of columns corresponding to the covariates
#' @param names names of the covariates in order of them listed
#' @param cluster name of clustering variable
#'
#' @return a dataframe of covariates linked to numerical ID's
#' @export
#'
#' @example inst/examples/example_define_covariates.R
#'
define_covariates <- function(data_frame, covariates_names, id, cluster = NULL){
  covariates <- data_frame %>% select(col_covariates)
  col_id <- deparse(substitute(id, environment()))
  col_ids <- deparse(substitute(id, environment()))
  id <- data_frame[, colnames(data_frame)==col_ids]

  covariate_df <- data.frame(id, covariates)
  if (!is.null(cluster)){
    col_cluster <- deparse(substitute(tx, environment()))
    cluster <- data_frame %>% select(starts_with(col_cluster))
    covariate_df <- cbind(covariate_df, cluster)
  }
  covariate_df <- cbind(id, covariate_df)
  structure(
    list(data_covariates = covariate_df,
         cluster = cluster
    ),
    class= "define_covariates")
}

#' Print Covariates
#'
#' @param x an [define_covariates()] object
#' @param ... additional arguments affecting the summary
#'   produced
#'
#' @export
#'
print.define_covariates <- function(x, ...){
  cat('\nThere are ', length(unique(x$data_covariates$id)), " unique individuals/observations in the dataset.")
  if (!is.null(x$cluster)){
    cat('\nThere are ', length(unique(x$data_covariates$cluster)), " different clusters.")
  }
  cat("\nThe first 5 lines of the dataset: \n")
  print(head(x$data_covariates))
}
