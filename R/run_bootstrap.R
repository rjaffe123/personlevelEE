#' Run bootstrap
#'
#' @param n
#' @param cost
#' @param effect
#' @param ID
#'
#' @return
#' @export
#'
#' @examples
#'
run_bootsrap <- function(n, cost, effect, ID){
  r_squared <- function(formula, data, indices) {
    val <- data[indices,] # selecting sample with boot
    fit <- lm(formula, data=val)
    return(fit$coefficients[2])
  }

  set.seed(1)

  bootstrap_data <- function(formula_cost, formula_effect, type){
    output_cost <- boot(data=data, statistic=r_squared,
                        R=1000, formula=formula_cost)
    output_effect <- boot(data=data, statistic=r_squared,
                          R=1000, formula=formula_effect)

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

}
