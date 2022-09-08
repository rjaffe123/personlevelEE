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
#' @return A [lm()] object and [dataframe()]
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
  return(list(data_effect, final_model))
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

  cat("\n", "There were", length(object[[1]]$id), "number of individuals in the control group.")
  cat("\n","There were", length(object[[1]]$id), "number of individuals in the treatment group.")
  cat("\n",'The average effect for the control group is: ', round(object[[2]]$coefficients[1], 3))
  cat("\n","The average effect for the treatment group is ", round(object[[2]]$coefficients[1] + object[[2]]$coefficients[2], 3))
  cat("\n","The incremental effect difference is: ", round(object[[2]]$coefficients[2], 3))

  cat("\n", 'The regression call is: Effect = beta0 + beta1(Treatment), where beta1 is a flag depending on the treatment status of the individual.')
  cat("\n", "The full OLS regression results are below. ")
  stargazer::stargazer(object[[2]], type="text", covariate.labels=c("Intercept (Control Average)", "Incremental Difference"),
                       omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.95, single.row=TRUE, intercept.bottom = FALSE)
}


#' Plot Define Effect Results
#'
#' @param object A result of [define_effect()]
#' @param type Type of plot
#'
#' @return plots
#' @export
#'
#' @examples
plot.define_effect <- function (object, type = c("regression", "barchart", "boxplot")){
  if (type == "regression"){
    autoplot(object[[2]], which = 1:6, label.size = 3)
  }
  else if (type == "barchart"){
    object[[1]] %>% group_by(tx) %>% summarize(average = mean(effect, na.rm=TRUE)) %>% ggplot(aes(fill = tx, y = average, x = tx)) + geom_bar(position="dodge", stat="identity") + labs(fill = "") +  ylab("Average Effect") + ggtitle("Average Effect for Each Group")
  }
  else if (type == "boxplot"){
    object[[1]] %>% ggplot(aes(x = tx, y = effect)) + geom_boxplot() + ggtitle("Distribution For Each Treatment Group") + xlab("") +ylab("Effect")
  }

}


