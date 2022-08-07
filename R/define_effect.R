#' Define Effect and Incremental Effect Difference
#'
#' The function will store values for effect, individual ID, and treatment to be used in other functions. Will evaluate the treatment difference
#' based on values given.
#'
#' A simple linear regression will evaluate the difference in effect between treatment status.
#'
#' @param effect Vector of effect values
#' @param id Vector of individual ID
#' @param tx Vector referring to the different treatment values associated with each individual ID (should be 2 different values)
#' @param treatment1 Value/Name associated to one treatment (control), default = 0
#' @param treatment2 Value/Name associated to the other treatment (comparator), default = 1
#'
#' @return A [lm()] object
#' @export
#'
#' @examples
define_effect <- function(effect, id, tx, control = 0, treatment = 1){
  ## errors:
  ## if treatment vector not 2 values
  if (nlevels(as.factor(tx))!=2){
    stop("Treatment vector cannot have more than 2 values.")
  }
  ## if effect & id not same length
  if (length(effect) != length(id)){
    stop("Effect and ID are not the same length")
  }
  ## if id values not numerical
  if (!is.numeric(id)){
    stop("IDs need to be a numerical vector")
  }
  ## perform linear regression
  data_effect <- data.frame(effect = effect, id = id, tx = tx)
  data_effect <- data_effect %>% mutate(tx = case_when(
                                              tx == control ~ "control",
                                              tx == treatment ~ "treatment"
  ))
  final_model <- lm(effect~tx, data = data_effect)
}

#' Summarize Define Effect Results
#'
#' Printing a formatted version of the regression results in context of an incremental cost effectiveness ratio for economic evaluations.
#'
#' To view raw regression results, see [summary()]
#' @param object a [define_effect()] object
#'
#' @return
#' @export
#'
#' @examples
print.define_effect <- function (object){
  ## format of print:
  ## regression values
  cat("There were", n, "number of individuals in the control, (", put name/value here,") group.")
  cat("There were", n, "number of individuals in the treatment, (", put name/value here,") group.")
  cat('The average effect for the control group is: ', )
  cat("The average effect for the treatment group is ", )
  cat("The incremental effect difference is: ", )

}

#' Plot Define Effect Results
#'
#' @param object A result of [define_effect()]
#' @param type Type of plot
#'
#' @return
#' @export
#'
#' @examples
plot.define_effect <- function (object, type = c("regression", "barchart")){

}


