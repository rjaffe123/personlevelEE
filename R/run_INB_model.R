#' Run Incremental Net Benefit Model
#'
#'The function will calculate the incremental net benefit based on the lambda from [define_NB()].
#'
#' @param covariates a [define_covariates()] object
#' @param nb_value a [define_nb()] object
#'
#' @return an object of the run_INB_model class
#' @export
#'
#' @examples examples/run_INB_model.R
run_INB_model <- function(nb_values, covariates = NULL) {
  # data for the model
  data <- nb_values$data
  model_list <- list()
  if (is.null(covariates)){
    data_lm <- data |> dplyr::select(-c(id, cost, effect))
    for (name in nb_values$column_names){
        lm1 <- lm(name ~ tx, data = data_lm)
        model_list[[name]] <- lm1
    }
  }
  else{
  data <- dplyr::left_join(data, covariates$data_covariates, by = c("id"))
  data_lm <- data |> dplyr::select(-c(id, cost, effect))
  for (name in nb_values$column_names){
    lm1 <- lapply(list(c("tx", covariates$names)), function(x) lm(reformulate(x, response = name), data = data_lm))
    lm1 <- lm1[[1]]
    model_list[[name]] <- lm1
  }
  }

  structure(
    list(data_inb = data,
         model_inb = model_list
    ),
    class = "run_inb_model"
  )
}

#' Plot INB model
#'
#' @param x a [run_INB_model()] object
#' @param type type of graph to plot, see more details
#' @param ... additional arguments affecting the summary
#'   produced.
#' @param bw Black & white plot theme for publications
#'
#' @return a [ggplot2()] object
#' @export
#'
plot.run_inb_model <- function (x, type = c("regression", "barchart", "boxplot", "ceac"), bw = FALSE,...){
  if (type == "regression"){
    cowplots <- list()
    y <- 1
    for (model in x$model_inb){
      p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
      p1<-p1+stat_smooth(method="loess", se = FALSE)+geom_hline(yintercept=0, col="red", linetype="dashed")
      p1<-p1+xlab("Fitted values")+ylab("Residuals")
      value =substring(names(x$model_inb)[y], regexpr("_", names(x$model_inb)[y]) + 1, nchar(names(x$model_inb)[y]))
      p1<-p1+labs(title = paste0("NB value = ", value), subtitle = "Residual vs Fitted Plot")


      p2 <- ggplot(model, aes(sample = .stdresid))+ stat_qq() + stat_qq_line()+
        xlab("Theoretical Quantiles")+
        ylab("Standardized Residuals")
      p2<-p2+ggtitle("Normal Q-Q")

      p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
      p3<-p3+stat_smooth(method="loess", na.rm = TRUE, se = FALSE)+xlab("Fitted Value")
      p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
      p3<-p3+ggtitle("Scale-Location")

      p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
      p5<-p5+stat_smooth(method="loess", na.rm=TRUE, se = FALSE)
      p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
      p5<-p5+ggtitle("Residual vs Leverage Plot")
      p5<-p5+labs(size = "")
      p5<-p5+theme(legend.position="none")

      cowplots[[names(x$model_inb)[y]]] <- cowplot::plot_grid(p1, p2, p3, p5, ncol = 1)
      y = y+1
    }
    res <- cowplot::plot_grid(plotlist = cowplots, ncol = length(x$model_inb))

  }
  else if (type == "barchart"){
    data_plot <- x$data_inb %>% tidyr::pivot_longer(cols = c(starts_with("nb_")), names_to = c("nb"))
    res <- data_plot %>% dplyr::group_by(tx, nb) |> dplyr::summarize(average = mean(value, na.rm=TRUE)) %>% ggplot(aes(x = nb, y = average, fill = tx)) +
      geom_bar(stat="identity", position = "dodge") + ggplot2::labs(fill = "") +
      ggplot2::ylab("Average Net Benefit") +
      ggplot2::ggtitle("Average Net Benefit for Each Group")+
      ggplot2::xlab("Net Benefit Value")
  }
  else if (type == "boxplot"){
    data_plot <- x$data_inb %>% tidyr::pivot_longer(cols = c(starts_with("nb_")), names_to = c("nb"))
    res <- data_plot %>% ggplot2::ggplot(aes(x = nb, y = value, fill = tx)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Distribution For Each Treatment Group") +
      ggplot2::xlab("Net Benefit Value") +
      ggplot2::ylab("Net Benefit") +
      ggplot2::labs(fill = "")
  }
  else if (type == "ceac"){
    y <- 1
    ceac_data <- data.frame(nb_values = c(), pvalues = c(), coeffs = c())
    for (model in x$model_inb){
        pvalue <- summary(model)$coefficients[,4][2]
        coeff <-model$coefficients[2]
        nb_value = substring(names(x$model_inb)[y], regexpr("_", names(x$model_inb)[y]) + 1, nchar(names(x$model_inb)[y]))
        temp <- data.frame(nb_values = nb_value, pvalues = pvalue, coeffs = coeff)
        ceac_data <- ceac_data |> rbind(temp)
        y = y+1
    }
    ceac_data <- ceac_data|> dplyr::mutate(prob = case_when(
      coeffs <0 ~ pvalues / 2,
      coeffs > 0 ~ 1 - pvalues/2
    ))

    res <- ceac_data |> ggplot2::ggplot(aes(x = as.numeric(nb_values), y = prob)) +geom_path()+
      ggplot2::ggtitle("CEAC from Regression Estimates of INB") +
      ggplot2::xlab(expression("WTP ("*lambda*")")) +
      ggplot2::ylab("Probability of CE")+
      ggplot2::theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))+
      geom_label(aes(label = round(prob, 3)))

  }
  if (bw & type != "regression") {
    res <- res +
      ggplot2::scale_color_grey(start = 0, end = .8) +
      scale_fill_grey(start = 0, end = .8)+
      theme_pub_bw()
  }
  else if (bw & type == "regression"){
    cowplots <- list()
    y <- 1
    for (model in x$model_inb){
      p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
      p1<-p1+stat_smooth(method="loess", se = FALSE)+geom_hline(yintercept=0, col="red", linetype="dashed")
      p1<-p1+xlab("Fitted values")+ylab("Residuals")
      value =substring(names(x$model_inb)[y], regexpr("_", names(x$model_inb)[y]) + 1, nchar(names(x$model_inb)[y]))
      p1<-p1+labs(title = paste0("NB value = ", value), subtitle = "Residual vs Fitted Plot")
      p1 <- p1 +ggplot2::scale_color_grey(start = 0, end = .8) +
        ggplot2::scale_fill_grey(start = 0, end = .8)+
        theme_pub_bw1()

      p2 <- ggplot(model, aes(sample = .stdresid))+ stat_qq() + stat_qq_line()+
        xlab("Theoretical Quantiles")+
        ylab("Standardized Residuals")
      p2<-p2+ggtitle("Normal Q-Q")
      p2 <- p2 +ggplot2::scale_color_grey(start = 0, end = .8) +
        ggplot2::scale_fill_grey(start = 0, end = .8)+
        theme_pub_bw1()

      p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
      p3<-p3+stat_smooth(method="loess", na.rm = TRUE, se = FALSE)+xlab("Fitted Value")
      p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
      p3<-p3+ggtitle("Scale-Location")
      p3 <- p3 +ggplot2::scale_color_grey(start = 0, end = .8) +
        ggplot2::scale_fill_grey(start = 0, end = .8)+
        theme_pub_bw1()

      p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
      p5<-p5+stat_smooth(method="loess", na.rm=TRUE, se = FALSE)
      p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
      p5<-p5+ggtitle("Residual vs Leverage Plot")
      p5<-p5+labs(size = "")
      p5 <- p5 + theme_pub_bw1()
      p5<-p5+theme(legend.position="none")


      cowplots[[names(x$model_inb)[y]]] <- cowplot::plot_grid(p1, p2, p3, p5, ncol = 1)
      y = y+1
    }
    res <- cowplot::plot_grid(plotlist = cowplots, ncol = length(x$model_inb))

  }
  res

}

#' Print INB regression results
#'
#' @param x an [run_INB_model()] object
#' @param ... additional arguments affecting the summary
#'   produced.
#'
#' @return returns formatted regression results
#' @export
#'
print.run_inb_model <- function(x, ...) {

  cat("\n", "The full INB regression results are below. ")
  stargazer::stargazer(nb_model$model_inb, type = "text", dep.var.labels = c(""), column.labels = as.character(nb_value$lambda),
                       dep.var.caption = "Dependent variable: net benefit value", omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.95, intercept.bottom = FALSE)

}

