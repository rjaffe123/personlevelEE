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

  structure(
    list(data = data_bootstrap_data,
         n = n
    ),
    class("run_bootstrap")
  )
}



#' Plot output of bootstrap
#'
#' @param object object of [run_bootstrap()]
#' @param type type of graph to show, either "cloud" or "ceac"
#'
#' @return a [ggplot2()] object
#' @export
#'
#' @examples
plot.run_bootstrap <- function(object, type = c("cloud", "ceac")){
  if (type == "cloud"){
  plot1 <- object %>% ggplot(aes(x=delta_e, y = delta_c)) + geom_point() + xlab(expression(Delta*"E"))+ylab(expression(Delta*"C"))+ geom_vline(xintercept = 0, color ="grey") +
    geom_hline(yintercept = 0, color = "grey") +theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))
  print(plot1)
  }
  if (type == "ceac"){
    boostrap_data_probs <- object[[1]] %>% summarize(sum_icer0k_n = sum(icer0k_n)/n(), sum_icer100_n = sum(icer100_n)/n(), sum_icer1k_n = sum(icer1k_n)/n(), sum_icer10k_n= sum(icer10k_n)/n())

    boostrap_data_probs <- boostrap_data_probs %>% pivot_longer(sum_icer0k_n:sum_icer10k_n)
    boostrap_data_probs <- boostrap_data_probs %>% mutate(wtp = rep_len(c(0,100,1000,10000), 12))

    plot2 <- boostrap_data_probs %>% ggplot(aes(x = wtp, y = value)) +geom_path() +theme_stata() +ggtitle("CEAC for ICER") +
      xlab(expression(lambda*" (WTP)")) +
      ylab("Prob of CE")+
      theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))
    print(plot2)
  }

}




