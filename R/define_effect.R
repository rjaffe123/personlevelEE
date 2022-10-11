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
#' @param control Value/Name associated to one treatment (control), default = 0
#' @param treatment Value/Name associated to the other treatment (comparator), default = 1
#'
#' @return A [lm()] object and [dataframe()]
#' @export
#'
#' @examples examples/define_effect.R
define_effect <- function(effect, id, tx, control = 0, treatment = 1) {
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
  data_effect <- data_effect |> dplyr::mutate(tx = dplyr::case_when(
                                              tx == control ~ "control",
                                              tx == treatment ~ "treatment"
  ))
  final_model <- lm(effect~tx, data = data_effect)

  data_control <- data_effect %>% dplyr::filter(tx == "control")
  data_trt <- data_effect %>% dplyr::filter(tx == "treatment")
  ttest <- stats::t.test(data_control$effect, data_trt$effect)

  structure(
    list(data_effect = data_effect,
          final_model = final_model,
         ttest = ttest
        ),
    class = "define_effect")


}

#' Summarize Define Effect Results
#'
#' Printing a formatted version of the regression results in context of an incremental cost effectiveness ratio for economic evaluations.
#'
#' To view raw regression results, see [summary()]
#'
#' @param x a [define_effect()] object
#' @param ... additional arguments affecting the summary
#'   produced.
#'
#' @return formatted regression results
#' @export
#'
#' @examples
print.define_effect <- function (x, ...){ ## add t test results

  cat("\n",'The average effect (std. dev) for the control group is: ', round(x$final_model$coefficients[1], 3), "(", sd(subset(x$data_effect, x$data_effect$tx =="control")$effect), ")")
  cat("\n","The average effect (std. dev) for the treatment group is ", round(x$final_model$coefficients[1] + x$final_model$coefficients[2], 3), "(", sd(subset(x$data_effect, x$data_effect$tx =="treatment")$effect), ")")
  cat("\n","The incremental effect difference (95% CI) is: ", round(x$final_model$coefficients[2], 3), "(", x$ttest$conf.int[1], ", ", x$ttest$conf.int[2], ")")
  cat("\n", "The p-value from a two sided t-test with an alternative hypothesis of true difference in means is not equal to 0 is: ", x$ttest$p.value)
  cat("\n", 'The regression call is: Effect = beta0 + beta1(Treatment), where beta1 is a flag depending on the treatment status of the individual.')
  cat("\n", "The full OLS regression results are below. ")
  stargazer::stargazer(x$final_model, type="text", covariate.labels=c("Intercept (Control Average)", "Incremental Difference"),
                       omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.95, single.row=TRUE, intercept.bottom = FALSE)
}


#' Plot Define Effect Results
#'
#' @param x A result of [define_effect()]
#' @param bw Black & white plot theme for publications
#' @param type Type of plot
#'
#' @return a [ggplot2()] object
#'
#' @examples  examples/define_effect.R
#'
#' @export
plot.define_effect <- function (x, type = c("regression", "barchart", "boxplot"), bw = FALSE, ...){
  if (type == "regression"){
    require(ggfortify)
    res <- ggplot2::autoplot(x$final_model, which = 1:6, label.size = 3)
  }
  else if (type == "barchart"){
    res <- x$data_effect |> dplyr::group_by(tx) |> dplyr::summarize(average = mean(effect, na.rm=TRUE)) |> ggplot2::ggplot(aes(fill = tx, y = average, x = tx)) +
      ggplot2::geom_bar(position="dodge", stat="identity") +
      ggplot2::labs(fill = "") +
      ggplot2::ylab("Average Effect") +
      ggplot2::xlab("Treatment Group")+
      ggplot2::ggtitle("Average Effect for Each Group")
  }
  else if (type == "boxplot"){
    res <- x$data_effect |> ggplot2::ggplot(aes(x = tx, y = effect)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Distribution For Each Treatment Group") +
      ggplot2::xlab("") +
      ggplot2::ylab("Effect")
  }
  if (bw) {
    res <- res +
      ggplot2::scale_color_grey(start = 0, end = .8) +
      ggplot2::scale_fill_grey(start = 0, end = .8)+
      theme_pub_bw()
  }

  res
}


