#' Run bootstrap
#'
#' Non-parametric uncertainty analysis: bootstrapping. This function will sample the ICER estimate n times.
#'
#' @param n number of repetitions for bootstrapping
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#'
#' @return a [dataframe()] of bootstrapped results
#' @export
#'
#' @examples
#'
run_bootstrap <- function(n, cost, effect){
  r_squared <- function(formula, data, indices) {
    val <- data[indices,] # selecting sample with boot
    fit <- lm(formula, data=val)
    return(fit$coefficients[2])
  }
  data <- cost[[1]]
  data <- data %>% left_join(effect[[1]], by = c("id"))
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

  boostrap_data <- boostrap_data %>% mutate(formula = "normal")
  boostrap_data <- boostrap_data %>% arrange(icer)
  cat("\n 95% Confidence Interval based on bootstrapping: ", "(", boostrap_data[round(.25*n, 0),3], ", ", boostrap_data[round(.75*n, 0),3], ")")
  return(boostrap_data)
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
  plot1 <- object %>% ggplot(aes(x=delta_e, y = delta_c)) + geom_point() + xlab(expression(Delta*"E"))+ylab(expression(Delta*"C"))+ geom_vline(xintercept = 0, color ="grey") +
    geom_hline(yintercept = 0, color = "grey") +theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))
  print(plot1)
}




