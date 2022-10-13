#' Run bootstrap
#'
#' Non-parametric uncertainty analysis: bootstrapping. This function will sample the ICER estimate n times.
#'
#' @param n number of repetitions for bootstrapping, default = 1000
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#' @param lambda_min minimum WTP threshold to calculate probability CE
#' @param lambda_max maximum WTP threshold to calculate probability CE
#' @param breaks number of breaks between minimum and maximum WTP, or custom vector
#'
#' @return a [dataframe()] of bootstrapped results
#' @export
#'
#' @example inst/examples/example_run_bootstrap.R
run_bootstrap <- function(n = 1000, cost, effect, lambda_min = 0, lambda_max = 10000, breaks = 5){
  r_squared <- function(formula, data, indices) {
    val <- data[indices,] # selecting sample with boot
    fit <- lm(formula, data=val)
    return(fit$coefficients[2])
  }
  data <- cost$data_cost
  data <- data |> dplyr::left_join(effect$data_effect, by = c("id", "tx"))
  output_cost <- boot::boot(data = data, statistic = r_squared, R = n, formula = summary(cost$final_model)$call$formula)
  output_effect <- boot::boot(data = data, statistic = r_squared, R = n, formula = summary(effect$final_model)$call$formula)
  boostrap_data <- output_cost[["t"]]
  boostrap_data <- boostrap_data |> cbind(output_effect[["t"]])
  colnames(boostrap_data) <- c("delta_c", "delta_e")
  boostrap_data <- as.data.frame(boostrap_data)
  boostrap_data <- boostrap_data |> dplyr::mutate(icer = delta_c/delta_e)

  if (length(breaks) == 1){
      lambda_break <- round(length(c(lambda_min:lambda_max)) / breaks, 0)
      lambda_interval <- seq(from = lambda_min, to = lambda_max, by = lambda_break)
      if (! lambda_max %in% lambda_interval){
        lambda_interval <- c(lambda_interval, lambda_max)
      }
  }
  else{
    ## check if min max is included / between
    lambda_interval <- breaks
  }

  for (x in lambda_interval){
    print(x)
    name <- paste0("icer_", x, "_n")
    boostrap_data <- boostrap_data |> mutate(!! name := ifelse((icer < x & delta_e > 0), 1,0))
  }
  boostrap_data <- boostrap_data |> dplyr::arrange(icer)
  cat("\n 95% Confidence Interval for ICER based on bootstrapping: ", "(", boostrap_data[round(.025*n, 0),"icer"], ", ", boostrap_data[round(.975*n, 0),"icer"], ")")

  structure(
    list(data = boostrap_data,
         n = n,
         lambda_interval = lambda_interval
    ),
    class = "run_bootstrap"
  )
}



#' Plot output of bootstrap
#'
#' @param x object of [run_bootstrap()]
#' @param bw black & white plot theme?
#' @param type type of graph to show, either "cloud" or "ceac"
#'
#' @return a [ggplot2()] object
#' @export
#'
#'@example inst/examples/example_run_bootstrap.R
plot.run_bootstrap <- function(x, type = c("cloud", "ceac"), bw = FALSE, ...){
  if (type == "cloud"){
  plot1 <- x$data |> ggplot2::ggplot(ggplot2::aes(x=delta_e, y = delta_c)) + ggplot2::geom_point() + ggplot2::xlab(expression(Delta*"E"))+ ggplot2::ylab(expression(Delta*"C"))+ ggplot2::geom_vline(xintercept = 0, color ="grey") +
    ggplot2::geom_hline(yintercept = 0, color = "grey") +ggplot2::theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))+
    ggplot2::ggtitle(expression("Bootstrapped Results of  " *Delta*"E & "*Delta*"C"))

  }
  if (type == "ceac"){
    boostrap_data_probs <- x$data |> dplyr::select(starts_with("icer_"))
    boostrap_data_probs <- x$data |> dplyr::summarize(across(everything(), ~ sum(.x)/n()))
    boostrap_data_probs <- boostrap_data_probs |> tidyr::pivot_longer(cols = starts_with("icer_"))
    boostrap_data_probs <- boostrap_data_probs |> dplyr::mutate(wtp = rep_len(c(x$lambda_interval), nrow(boostrap_data_probs)))
    x <- rep(0, ncol(boostrap_data_probs))
    boostrap_data_probs <- x |> rbind(boostrap_data_probs)

    plot1 <- boostrap_data_probs |> ggplot2::ggplot(aes(x = wtp, y = value)) +ggplot2::geom_path() +
      ggplot2::ggtitle(expression("CEAC from Bootstrap Estimates of  " *Delta*"E & "*Delta*"C")) +
      ggplot2::xlab(expression("WTP ("*lambda*")")) +
      ggplot2::ylab("Probability of CE")+
      ggplot2::theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))+
      ggplot2::geom_label(aes(label = value))+
      ggplot2::ylim(0,1)
  }
  if (bw) {
    plot1<- plot1 +
      ggplot2::scale_color_grey(start = 0, end = .8) +
      scale_fill_grey(start = 0, end = .8)+
      theme_pub_bw()
  }
  plot1
}




