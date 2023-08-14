#' Define Cost and Incremental Cost Difference
#'
#' The function will store values for cost, individual ID, and treatment to be used in other functions. Will evaluate the treatment difference
#' based on values given.
#'
#' A simple linear regression will evaluate the difference in cost between treatment status.
#'
#' @param data_frame a data frame that contains the following information
#' @param cost (string) name of column that has cost data
#' @param id (string) name of ID column
#' @param tx (string) name of treatment column
#' @param control Value/Name associated to one treatment (control), default = 0
#' @param treatment Value/Name associated to the other treatment (comparator), default = 1
#'
#' @return a list of the regression, ttest objects and associated dataframe
#' @export
#'
#' @example inst/examples/example_define_cost.R
define_cost <- function(data_frame, cost, id, tx, control = 0, treatment = 1){
  col_cost <- deparse(substitute(cost, environment()))
  cost<- data_frame[, colnames(data_frame)==col_cost]
  col_ids <- deparse(substitute(id, environment()))
  #col_ids <- toString(id)
  id <- data_frame[, colnames(data_frame)==col_ids]
  col_tx <- deparse(substitute(tx, environment()))
  #col_tx <- toString(tx)
  tx <- data_frame[, colnames(data_frame)==col_tx]
  data_cost <- data.frame(cost = cost, id = id, tx = tx)
  # ## errors:
  # ## if treatment vector not 2 values
  # if (nlevels(as.factor(tx))>2){
  #   stop("Treatment vector cannot have more than 2 values.")
  # }
  # ## if effect & id not same length
  # if (length(data_cost$cost) != length(data_cost$id)){
  #   stop("Cost and ID are not the same length")
  # }
  # ## if id values not numerical
  # if (!is.numeric(data_cost$id)){
  #   stop("IDs need to be a numerical vector")
  # }
  ## perform linear regression
  data_cost <- data_cost |> dplyr::mutate(tx = dplyr::case_when(
    tx == control ~ "control",
    tx == treatment ~ "treatment"
  ))
  data_control <- data_cost %>% dplyr::filter(tx == "control")
  data_trt <- data_cost %>% dplyr::filter(tx == "treatment")
  ttest <- stats::t.test(data_control$cost, data_trt$cost)

  final_model <- lm(cost~tx, data = data_cost)
  structure(
    list(data_cost = data_cost,
         final_model = final_model,
         ttest = ttest
    ),
    class= "define_cost")

}

#' Summarize Define Cost Results
#'
#' Printing a formatted version of the regression results in context of an incremental cost effectiveness ratio for economic evaluations.
#'
#' To view raw regression results, see [summary()]
#'
#' @param x a [define_cost()] object
#' @param ... additional arguments affecting the summary
#'   produced.
#'
#'
#' @return A formatted regression results
#' @export

print.define_cost <- function(x, ...){

  cat("\n",'The average cost for the control group is: ', round(x$final_model$coefficients[1], 3), "(", sd(subset(x$data_cost, x$data_cost$tx =="control")$cost), ")")
  cat("\n","The average cost for the treatment group is ", round(x$final_model$coefficients[1] + x$final_model$coefficients[2], 3), "(", sd(subset(x$data_cost, x$data_cost$tx =="treatment")$cost), ")")
  cat("\n","The incremental effect difference (95% CI) is: ", round(x$final_model$coefficients[2], 3), "(", confint(x$final_model)[2], ", ", confint(x$final_model)[4], ")")
  cat("\n", "The p-value from a two sided t-test with an alternative hypothesis of true difference in means is not equal to 0 is: ", x$ttest$p.value)
  cat("\n", 'The regression call is: Cost = beta0 + beta1(Treatment), where beta1 is a flag depending on the treatment status of the individual.')
  cat("\n", "The full OLS regression results are below. ")
  stargazer::stargazer(x$final_model, type="text", covariate.labels=c("Intercept (Control Average)", "Incremental Difference"),
                       omit.stat=c("LL","ser","f"), ci.custom = list(confint(x$final_model)), single.row=TRUE, intercept.bottom = FALSE)
}



#' Plot Define Cost Results
#'
#' @param x A result of [define_cost()]
#' @param type Type of plot
#' @param ... additional arguments affecting the plot
#'   produced.
#' @param bw Black & white plot theme for publications
#'
#' @return a [ggplot2()] object
#' @export
#'
#' @example  inst/examples/example_define_cost.R
plot.define_cost <- function (x, type = c("regression", "barchart", "boxplot"), bw = FALSE, ...){
  if (type == "regression"){
    # require(ggfortify, quietly = TRUE)
    # res <- ggplot2::autoplot(x$final_model, which = 1:6, label.size = 3)
    options(warn = -1)
    res <- cowplot::plot_grid(plotlist = personlevelEE::plot_regression(x$final_model), ncol = 2)
    options(warn = 0)
  }
  else if (type == "barchart"){
   res <- x$data_cost |> dplyr::group_by(tx) |> dplyr::summarize(average = mean(cost, na.rm=TRUE)) |> ggplot2::ggplot(aes(fill = tx, y = average, x = tx)) +
      ggplot2::geom_bar(position="dodge", stat="identity") +
      ggplot2::labs(fill = "") +
      ggplot2::ylab("Average Cost") +
      ggplot2::xlab("Treatment Group")+
      ggplot2::ggtitle("Average Cost for Each Group")
  }
  else if (type == "boxplot"){
    res <- x$data_cost |> ggplot2::ggplot(aes(x = tx, y = cost)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Distribution For Each Treatment Group") +
      ggplot2::xlab("") +
      ggplot2::ylab("Cost")
  }
  if (bw & type != "regression") {
    res <- res +
      ggplot2::scale_color_grey(start = 0, end = .8) +
      ggplot2::scale_fill_grey(start = 0, end = .8)+
      theme_pub_bw()
  }
  else if (bw & type == "regression") {
    options(warn = -1)
    res <- cowplot::plot_grid(plotlist = personlevelEE::plot_regression_bw(x$final_model), ncol = 2)
    options(warn = 0)
  }

  res

}
