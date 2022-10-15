#' Run ICER model
#'
#' This function will calculate an ICER based on the cost and effect regressions.
#' It will consider the different covariates and interaction terms from your data in the model.
#'
#' @param cost result of [define_cost()]
#' @param effect result of [define_effect()]
#' @param covariates a [define_covariates()] object
#' @param method type of model ("linear", "hierarchical"), default = "linear"
#' @param interaction boolean, if model should consider interaction terms, default = FALSE
#'
#' @return a list containing the regression results for cost, effect, related data frames, and final icer values
#' @export
#'
#' @example inst/examples/example_run_icer_model.R
run_icer_model <- function(cost, effect, covariates = NULL, interaction = FALSE, method = "linear"){

  if (is.null(covariates)){ ## no covariates
    data_cost <- cost$data_cost
    data_effect<- effect$data_effect

    if (method == "linear"){
      data_lm_cost <- data_cost |> dplyr::select(-c(id))
      data_lm_effect <- data_effect |> dplyr::select(-c(id))
      cost_lm <- lapply(list(c("tx")), function(x) lm(reformulate(x, response = "cost"), data = data_lm_cost))
      effect_lm <- lapply(list(c("tx")), function(x) lm(reformulate(x, response = "effect"), data = data_lm_effect))


      # if (interaction == TRUE){## no covariates + linear model + interaction terms
      #   interaction_terms <- do.call(paste, c(as.list(cvoariates$names), sep = ":"))
      #   cost_lm <- lapply(list(c("tx", covariate$names, interaction_terms)), function(x) lm(reformulate(x, response = "cost"), data = data))
      #   effect_lm <- lapply(list(c("tx", covariate$names, interaction_terms)), function(x) lm(reformulate(x, response = "effect"), data = data))
      # }
    }
  }

  else{ ## covariates
    data_cost <- dplyr::left_join(cost$data_cost, covariates$data_covariates, by = c("id"))
    data_effect <- dplyr::left_join(effect$data_effect, covariates$data_covariates, by = c("id"))
    data_total <- data_cost |> dplyr::left_join(data_effect, by = c("id", "tx"))

    if (method == "linear"){
      data_lm_cost <- data_cost |> dplyr::select(-c(id))
      data_lm_effect <- data_effect |> dplyr::select(-c(id))
      if (interaction == TRUE){## covariates + linear model + interaction terms
        interaction_terms <- do.call(paste, c(as.list(covariates$names), sep = ":"))
        cost_lm <- lapply(list(c("tx", covariates$names, interaction_terms)), function(x) lm(reformulate(x, response = "cost"), data = data_lm_cost))
        effect_lm <- lapply(list(c("tx", covariates$names, interaction_terms)), function(x) lm(reformulate(x, response = "effect"), data = data_lm_effect))
      }
      else {## covariates + linear model
        cost_lm <- lapply(list(c("tx", covariates$names)), function(x) lm(reformulate(x, response = "cost"), data = data_lm_cost))
        effect_lm <- lapply(list(c("tx", covariates$names)), function(x) lm(reformulate(x, response = "effect"), data = data_lm_effect))
      }
  }
    cost_lm <- cost_lm[[1]]
    effect_lm <- effect_lm[[1]]

    ## calculate ICER
    incremental_cost <- cost_lm$coefficients[2]
    incremental_effect <- effect_lm$coefficients[2]
    icer <- incremental_cost / incremental_effect
    cat("The ICER is: ", icer)
  }


  structure(
    list(cost_lm = cost_lm,
         effect_lm = effect_lm,
         icer = icer,
         data_total = data_total,
         data_effect = data_effect,
         data_cost = data_cost
    ),
    class = "run_icer_model"
  )
}


#' Print ICER regression results
#'
#' @param x an [run_icer_model()] object
#' @param ... additional arguments affecting the plot
#'   produced.
#'
#' @return returns formatted table of regression results
#' @export
#'
print.run_icer_model <- function(x, ...) {

  cat("\n", "The full ICER regression(s) results are below. ")
  stargazer::stargazer(x$cost_lm, x$effect_lm, type="text",
                       omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.95, intercept.bottom = FALSE, dep.var.labels = c(""), column.labels = c("Cost", "Effect"))

}

#' Print a summary table of ICER
#'
#' @param object a [run_icer_model()] object
#' @param ... additional arguments affecting the summary produced.
#'
#' @return a tibble
#' @export
#'
summary.run_icer_model <- function(object, ...) {
  tx1_name <- names(table(object$data_total$tx)[1])
  tx2_name <- names(table(object$data_total$tx)[2])
  means_tx1 <- object$data_total |> dplyr::filter(tx == tx1_name) |> dplyr::summarize(mean_cost = mean(cost, na.rm = TRUE), mean_effect = mean(effect, na.rm = TRUE))
  means_tx2 <- object$data_total |> dplyr::filter(tx == tx2_name) |> dplyr::summarize(mean_cost = mean(cost, na.rm = TRUE), mean_effect = mean(effect, na.rm = TRUE))

  tb <- tibble::tibble("strategy" = c(tx1_name, tx2_name),
               "average cost" = c(round(means_tx1$mean_cost,3), round(means_tx2$mean_cost,3)),
              "incremental cost" = c("----", round(object$cost_lm$coefficients[2],3)),
              "average effect" = c(round(means_tx1$mean_effect,3), round(means_tx2$mean_effect, 3)),
              "incremental effect" = c("----", round(object$effect_lm$coefficients[2], 3)),
              "ICER" = c("----", round(object$icer, 3))
              )
  tb
}

#' Plot ICER regressions
#'
#' @param x an [run_icer_model()] object
#' @param type type of graph, default = regression diagnostics
#' @param ... additional arguments affecting the summary
#'   produced.
#' @param bw Black & white plot theme for publications
#'
#' @return a [ggplot()] object
#' @export
#'
#'
plot.run_icer_model <- function(x, type = c("regression"), bw = FALSE, ...){
  if (type == "regression"){
    #require(ggfortify, quietly = TRUE)
    # p1 <-ggplot2::autoplot(x$cost_lm, which = c(1:3, 5), label.size = 1)
    # p2 <-ggplot2::autoplot(x$effect_lm, which = c(1:3, 5), label.size = 1)
    # first_graph_cost <- p1[[1]] + ggplot2::labs(title = "COST", subtitle = "Residuals vs. Fitted") +
    #   ggplot2::theme(plot.title = element_text(size = 18),plot.subtitle = element_text(size = 14))
    # first_graph_effect <- p2[[1]] + ggplot2::labs(title = "EFFECT", subtitle = "Residuals vs. Fitted") +
    #   ggplot2::theme(plot.title = element_text(size = 18),plot.subtitle = element_text(size = 14))
    #
    # res <- cowplot::plot_grid(first_graph_cost, first_graph_effect, p1[[2]], p2[[2]],p1[[3]], p2[[3]],p1[[4]], p2[[4]],ncol = 2)
    p1 <- personlevelEE::plot_regression(x$cost_lm)
    p2 <- personlevelEE::plot_regression(x$effect_lm)
    first_graph_cost <- p1[["residvfitted"]] + ggplot2::labs(title = "COST", subtitle = "Residuals vs. Fitted") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18),plot.subtitle = ggplot2::element_text(size = 14))
    first_graph_effect <- p2[["residvfitted"]] + ggplot2::labs(title = "EFFECT", subtitle = "Residuals vs. Fitted") +
         ggplot2::theme(plot.title = ggplot2::element_text(size = 18),plot.subtitle = ggplot2::element_text(size = 14))
    res <- cowplot::plot_grid(first_graph_cost, first_graph_effect, p1[[2]], p2[[2]],p1[[3]], p2[[3]],p1[[4]], p2[[4]],ncol = 2)


  }
  if (bw) {
    p1 <- personlevelEE::plot_regression_bw(x$cost_lm)
    p2 <- personlevelEE::plot_regression_bw(x$effect_lm)
    first_graph_cost <- p1[["residvfitted"]] + ggplot2::labs(title = "COST", subtitle = "Residuals vs. Fitted") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18),plot.subtitle = ggplot2::element_text(size = 14))
    first_graph_effect <- p2[["residvfitted"]] + ggplot2::labs(title = "EFFECT", subtitle = "Residuals vs. Fitted") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18),plot.subtitle = ggplot2::element_text(size = 14))
    res <- cowplot::plot_grid(first_graph_cost, first_graph_effect, p1[[2]], p2[[2]],p1[[3]], p2[[3]],p1[[4]], p2[[4]],ncol = 2)
  }
  res
}
