#' Define Covariates
#'
#' Takes a list of covariates and stores them for future use.
#'
#' @param data_frame a data frame that contains the following information
#' @param id (string) name of ID column
#' @param covariate_names a list of the names of columns corresponding to the covariates
#' @param cluster name of clustering variable
#'
#' @return a dataframe of covariates linked to numerical ID's
#' @export
#'
#' @example inst/examples/example_define_covariates.R
#'
define_covariates <- function(data_frame, covariates_names, id, cluster = "none"){
  covariates <- data_frame %>%dplyr::select(covariates_names)
  #col_ids <- toString(id)
  col_ids <- deparse(substitute(id, environment()))
  id <- data_frame[, colnames(data_frame)==col_ids]
  covariate_df <- data.frame(id, covariates)

  if (cluster != "none"){
    cluster_col <- data_frame %>%dplyr::select(cluster)
    covariate_df <- cbind(covariate_df, cluster_col)
  }

  full_df <- data_frame
  structure(
    list(data_covariates = covariate_df,
         covariatenames = covariates_names,
         cluster = cluster,
         fulldf = full_df
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
  cat('\nThere are ', length(unique(x$data_covariates$id)), " unique individuals/observations in the dataset. \n")
  if (!is.null(x$cluster)){
    cat('There are ', length(unique(x$data_covariates$cluster)), " different clusters. \n\n")
  }
  tab1 <- tableone::CreateTableOne(vars = x$covariatenames, strata = "tx", data = x$fulldf)
  print(tab1)
}
