#' Run bootstrap
#'
#' Non-parametric uncertainty analysis: bootstrapping. This function will sample the ICER estimate n times.
#'
#' @param n number of repetitions for bootstrapping
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#'
#' @return a dataframe of bootstrapped results
#' @export
#'
#' @examples
#'
run_bootsrap <- function(n, cost, effect){
  data <- cost[[1]]
  data <- data %>% left_join(effect[[1]], by = c("id", "tx"))
  output_cost <- boot(data = data, statistic = r_squared, R = n, formula = summary(cost[[2]])$call$formula)
  output_effect <- boot(data = data, statistic = r_squared, R = n, formula = summary(effect[[2]])$call$formula)
  boostrap_data <- output_cost[["t"]]
  boostrap_data <- boostrap_data %>% cbind(output_effect[["t"]])
  colnames(boostrap_data) <- c("delta_c", "delta_e")
  boostrap_data <- as.data.frame(boostrap_data)
  boostrap_data <- boostrap_data %>% mutate(icer = delta_c/delta_e)

  boostrap_data <- boostrap_data %>% mutate(icer0k_n = ifelse((icer < 0 & delta_e > 0), 1,0),
                                            icer100_n = ifelse((icer < 100 & delta_e > 0),1,0),
                                            icer1k_n = ifelse((icer < 1000 & delta_e > 0),1,0),
                                            icer10k_n = ifelse((icer < 10000 & delta_e > 0),1,0))

  boostrap_data <- boostrap_data %>% mutate(formula = type)

}



#' R-squared function
#'
#' @param formula
#' @param data
#' @param indices
#'
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
r_squared <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot
  fit <- lm(formula, data=val)
  return(fit$coefficients[2])
}


#' Plot bootstrap
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
plot.run_bootstrap <- function(object){

}


#' Summart of Bootstrap
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
summary.run_bootstrap <- function(object){

}

