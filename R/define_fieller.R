#' Run Fieller's theorem
#'
#' Function uses Fieller's theorem to define values to create a confidence intervals and related confidence ellipses.
#'
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#'
#' @return a data frame containing the data to create confidence ellipses
#' @export
#'
#' @example inst/examples/example_define_fieller.R
define_fieller <- function(cost, effect) {
  cost_model <- cost$final_model
  effect_model <- effect$final_model

  data <- dplyr::left_join(cost$data_cost, effect$data_effect, by = c("id", "tx"))
  n0 <- table(cost$data_cost$tx)[1]
  n1 <- table(effect$data_effect$tx)[2]

  ellipse_data <- data |> dplyr::mutate(dc = cost_model$coefficients[2], se_dc = summary(cost_model)$coefficients[2,2]) ## coef of treatment & standard error
  ellipse_data <- ellipse_data |> dplyr::mutate(de = effect_model$coefficients[2], se_de = summary(effect_model)$coefficients[2,2]) ## coef of treatment & std error

  rho_0 <- stats::cor(subset(data, data$tx == "control")$cost,subset(data, data$tx == "control")$effect, use='complete.obs')
  rho_1 <- stats::cor(subset(data, data$tx == "treatment")$cost,subset(data, data$tx == "treatment")$effect, use='complete.obs')
  ellipse_data <- ellipse_data |> dplyr::mutate(rho_0 = rho_0, rho_1=rho_1)

  t.test_cost <- stats::t.test(subset(data, data$tx == "control")$cost,subset(data, data$tx == "treatment")$cost)
  t.test_effect <- stats::t.test(subset(data, data$tx == "control")$effect,subset(data, data$tx == "treatment")$effect)

  ellipse_data <- ellipse_data |> dplyr::mutate(se_c0 = sd(subset(data, data$tx == "control")$cost),
                                                se_c1 = sd(subset(data, data$tx == "treatment")$cost),
                                                se_e0 = sd(subset(data, data$tx == "control")$effect),
                                                se_e1 = sd(subset(data, data$tx == "treatment")$effect),
                                                df = t.test_cost$parameter,
                                                n0 = n0,
                                                n1 = n1)

  ellipse_data <- ellipse_data |> dplyr::mutate(cov = (rho_1*se_c1*se_e1) / n1 + (rho_0*se_c0*se_e0) / n0)
  ellipse_data <- ellipse_data |> dplyr::mutate(corr = cov / (se_dc*se_de))

  ##95
  ellipse_data <- ellipse_data |> dplyr::mutate(theta = pi*2*(id-1)/(n0 + n1 - 1))
  ellipse_data <- ellipse_data |> dplyr::mutate(part1_c95 = sqrt(-2*log(1-.95))*se_dc,
                                          part1_e95 = sqrt(-2*log(1-.95))*se_de)

  ellipse_data <- ellipse_data |> dplyr::mutate(part2_c = cos(theta-acos(corr)/2), part2_e = cos(theta+acos(corr)/2))
  ellipse_data <- ellipse_data |> dplyr::mutate(delta_c95= part1_c95*part2_c + dc,
                                          delta_e95 = part1_e95*part2_e + de)
  ##75
  ellipse_data <- ellipse_data |> dplyr::mutate(part1_c75 = sqrt(-2*log(1-.75))*se_dc,
                                          part1_e75 = sqrt(-2*log(1-.75))*se_de)

  ellipse_data <- ellipse_data |> dplyr::mutate(delta_c75= part1_c75*part2_c + dc,
                                          delta_e75 = part1_e75*part2_e + de)
  #50
  ellipse_data <- ellipse_data |> dplyr::mutate(part1_c50 = sqrt(-2*log(1-.50))*se_dc,
                                          part1_e50 = sqrt(-2*log(1-.50))*se_de)

  ellipse_data <- ellipse_data |> dplyr::mutate(delta_c50= part1_c50*part2_c + dc,
                                          delta_e50 = part1_e50*part2_e + de)

  ellipse_data <- ellipse_data |> dplyr::mutate(icer = dc/de)

  ## confidence intervals
  ellipse_data <- ellipse_data |> dplyr::mutate(t = qt(p=.025, df=df)) ## get t value for df and alpha of .025
  ellipse_data <- ellipse_data |> dplyr::mutate(MM = de*dc - t^2*corr*se_de*se_dc)
  ellipse_data <- ellipse_data |> dplyr::mutate(NN = de^2 - t^2*se_de^2)
  ellipse_data <- ellipse_data |> dplyr::mutate(OO = dc^2 - t^2*se_dc^2)

  ellipse_data <- ellipse_data |> dplyr::mutate(ll = (MM - (MM^2 - NN*OO)^0.5) / NN)
  ellipse_data <- ellipse_data |> dplyr::mutate(ul = (MM + (MM^2 - NN*OO)^0.5) / NN)
  cat("The 95% confidence interval based on Fieller's Theorm is: (", ellipse_data$ll[1], ", ", ellipse_data$ul[1], ")")


  structure(
    list(data = ellipse_data
    ),
    class= "define_fieller"
  )

}

#' Plot Confidence Ellipses
#'
#' @param x a [define_fieller()] object
#' @param ... additional arguments affecting the plot
#'   produced.
#' @param CI boolean, add confidence interval lines
#' @param bw & white plot theme for publications
#'
#' @return a [ggplot2()] object
#' @export
#'
#' @example inst/examples/example_define_fieller.R
plot.define_fieller <- function(x, CI = FALSE, bw = FALSE, ...){
    if (CI){
    label <- data.frame(
        yvalue = c(x$data$ul[1]*6, x$data$ll[1]*4),
        xvalue = c(mean(x$data$delta_e75, na.rm = TRUE), mean(x$data$delta_e75, na.rm=TRUE)/2),
        label = c(paste0("UL = ", round(x$data$ul[1], 2)), paste0("LL = ", round(x$data$ll[1], 2)))
    )
    plot1<-x$data |> ggplot2::ggplot() +ggplot2::geom_path(aes(x = delta_e95, y = delta_c95, color = "red")) +
    ggplot2::geom_path(aes(x = delta_e75, y = delta_c75, color = "blue")) +
    ggplot2::geom_path(aes(x = delta_e50, y = delta_c50, color = "green")) +
    ggplot2::geom_point(aes(x = de, y = dc), color = "black") +
    ggplot2::geom_vline(xintercept = 0, color ="grey") +
    ggplot2::geom_hline(yintercept = 0, color = "grey") +
    ggplot2::xlab(expression(Delta*"E"))+ggplot2::ylab(expression(Delta*"C"))+
    ggplot2::scale_colour_manual(name = 'Confidence Interval',
                        values =c('red'='red','blue'='blue','green'='green'), labels = c('95%','75%','50%')) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1))+
    ggplot2::geom_abline(slope = x$data$ul[1], linetype="dotted", show.legend = TRUE)+
    ggplot2::geom_abline(slope = x$data$ll[1], linetype="dotted", show.legend = TRUE)+
    ggplot2::geom_label(data = label, aes(label = label, x = xvalue, y = yvalue))
    }
    else{
      plot1<-x$data |> ggplot2::ggplot() +ggplot2::geom_path(aes(x = delta_e95, y = delta_c95, color = "red")) +
        ggplot2::geom_path(aes(x = delta_e75, y = delta_c75, color = "blue")) +
        ggplot2::geom_path(aes(x = delta_e50, y = delta_c50, color = "green")) +
        ggplot2::geom_point(aes(x = de, y = dc), color = "black") +
        ggplot2::geom_vline(xintercept = 0, color ="grey") +
        ggplot2::geom_hline(yintercept = 0, color = "grey") +
        ggplot2::xlab(expression(Delta*"E"))+ggplot2::ylab(expression(Delta*"C"))+
        ggplot2::scale_colour_manual(name = 'Confidence Interval',
                                     values =c('red'='red','blue'='blue','green'='green'), labels = c('95%','75%','50%')) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1))

    }
  if (bw) {
    plot1 <- plot1 +
      #ggplot2::scale_colour_manual(name = 'Confidence Interval',
                                   #values =c('red'='red','blue'='blue','green'='green'), labels = c('95%','75%','50%')) +
      #ggplot2::scale_fill_grey(start = 0, end = .8)+
      theme_pub_bw()
  }

    plot1
}


#' Print NB data
#'
#' @param x an [define_fieller()] object
#' @param ... additional arguments affecting the print
#'
#' @export
#'
print.define_fieller <- function(x, ...){
  cat("The 95% confidence interval based on Fieller's Theorm is: (", x$data$ll[1], ", ", x$data$ul[1], ")")
}
