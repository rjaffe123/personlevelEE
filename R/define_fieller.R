#' Run Fieller's theorem
#'
#' Function uses Fieller's theorem to define values to create a confidence intervals and related confidence ellipses.
#'
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#'
#' @return
#' @export
#'
#' @examples
define_fieller <- function(cost, effect) {
  data <- left_join(cost[[1]], effect[[1]], by = c("id", "tx"))
  n0 <- table(cost[[1]]$tx)[1]
  n1 <- table(effect[[1]]$tx)[2]
  ellipse_data <- ellipse_data %>% mutate(dc = cost[[2]]$coefficients[2], se_dc = summary(cost[[2]])$coefficients[2,2]) ## coef of treatment & standard error
  ellipse_data <- ellipse_data %>% mutate(de = effect[[2]]$coefficients[2], se_de = summary(effect[[2]])$coefficients[2,2]) ## coef of treatment & std error
  rho_0 <- cor(subset(data, data$tx == 0)$cost,subset(data, data$tx == 0)$effect)
  rho_1 <- cor(subset(data, data$tx == 1)$cost,subset(data, data$tx == 1)$effect)
  ellipse_data <- ellipse_data %>% mutate(rho_0 = rho_0, rho_1=rho_1)

  t.test_cost <- t.test(subset(data, data$tx == 0)$cost,subset(data, data$tx == 1)$cost)
  t.test_effect <- t.test(subset(data, data$tx == 0)$effect,subset(data, data$tx == 1)$effect)

  ellipse_data <- ellipse_data %>% mutate(se_c0 = sd(subset(data, data$tx == 0)$cost), se_c1 = sd(subset(data, data$tx == 1)$cost), se_e0 = sd(subset(data, data$tx == 0)$effect), se_e1 = sd(subset(data, data$tx == 1)$effect), df = t.test_cost$parameter, n0 = n0, n1 = n1)

  ellipse_data <- ellipse_data %>% mutate(cov = (rho_1*se_c1*se_e1) / n1 + (rho_0*se_c0*se_e0) / n0)
  ellipse_data <- ellipse_data %>% mutate(corr = cov / (se_dc*se_de))

  ##95
  ellipse_data <- ellipse_data %>% mutate(theta = pi*2*(id-1)/(n0 + n1 - 1))
  ellipse_data <- ellipse_data %>% mutate(part1_c95 = sqrt(-2*log(1-.95))*se_dc,
                                          part1_e95 = sqrt(-2*log(1-.95))*se_de)

  ellipse_data <- ellipse_data %>% mutate(part2_c = cos(theta-acos(corr)/2), part2_e = cos(theta+acos(corr)/2))
  ellipse_data <- ellipse_data %>% mutate(delta_c95= part1_c95*part2_c + dc,
                                          delta_e95 = part1_e95*part2_e + de)
  ##75
  ellipse_data <- ellipse_data %>% mutate(part1_c75 = sqrt(-2*log(1-.75))*se_dc,
                                          part1_e75 = sqrt(-2*log(1-.75))*se_de)

  ellipse_data <- ellipse_data %>% mutate(delta_c75= part1_c75*part2_c + dc,
                                          delta_e75 = part1_e75*part2_e + de)
  #50
  ellipse_data <- ellipse_data %>% mutate(part1_c50 = sqrt(-2*log(1-.50))*se_dc,
                                          part1_e50 = sqrt(-2*log(1-.50))*se_de)

  ellipse_data <- ellipse_data %>% mutate(delta_c50= part1_c50*part2_c + dc,
                                          delta_e50 = part1_e50*part2_e + de)

  ellipse_data <- ellipse_data %>% mutate(icer = dc/de)
  return(ellipse_data)
  cat("\n The 95% confidence interval: " )
  cat("\n The 75% confidence interval: " )
  cat("\n The 50% confidence interval: " )

}

#' Plot Confidence Ellipses
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
plot.define_fieller <- function(object){
    ellipse_lm_basic %>% ggplot() +geom_path(aes(x = delta_e95, y = delta_c95, color = "red")) +
    geom_path(aes(x = delta_e75, y = delta_c75, color = "blue")) +
    geom_path(aes(x = delta_e50, y = delta_c50, color = "green")) +
    geom_point(aes(x = de, y = dc), color = "black") +
    geom_vline(xintercept = 0, color ="grey") +
    geom_hline(yintercept = 0, color = "grey") +
    xlab(expression(Delta*"E"))+ylab(expression(Delta*"C"))+
    scale_colour_manual(name = 'Confidence Interval',
                        values =c('red'='red','blue'='blue','green'='green'), labels = c('95%','75%','50%')) + theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))
}


#' Print Confidence
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
print.define_fieller <- function(object){

}
