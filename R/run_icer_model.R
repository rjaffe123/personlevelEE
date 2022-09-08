#' Run ICER model
#'
#' This function will
#'
#' @param cost result of [define_cost()]
#' @param effect result of [define_effect()]
#' @param covariates a dataframe of covariates as defined by [define_covariates()]
#' @param clustering clustering variable, default = NA
#' @param method type of model ("linear", "hierarchical")
#' @param interaction TRUE, if model should consider interaction terms
#'
#' @return
#' @export
#'
#' @examples
run_icer_model <- function(cost, effect, covariates = NA, interaction = TRUE, method = "linear"){
  data_cost <- left_join(cost[[1]], covariates, by = c("id"))
  data_effect <- left_join(effect[[1]], covariates, by = c("id"))

  if (method == "linear"){
    data_lm_cost <- data_cost %>% select(-c(id))
    data_lm_effect <- data_effect %>% select(-c(id))
    if (interaction == TRUE){
      cost_lm <- lm(cost ~ tx + . * ., data = data_lm_cost)
      effect_lm <- lm(effect ~ tx + . * ., data = data_lm_effect)
    }
    else {
      cost_lm <- lm(cost ~ tx + ., data = data_cost)
      effect_lm <- lm(effect ~ tx + ., data = data_cost)
    }
    ## calculate ICER
    incremental_cost <- cost_lm$coefficients[2]
    incremental_effect <-effect_lm$coefficients[2]
    icer <- incremental_cost / incremental_effect
    cat("The ICER is: ", icer)
  }
  return(list(cost_lm, effect_lm, icer))

}


#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
print.run_icer_model <- function(object) {

  cat("\n", "The full ICER regression results are below. ")
  stargazer::stargazer(object[[1]], object[[2]], type="text",
                       omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.95, intercept.bottom = FALSE)

}

#' Title
#'
#' @param object
#' @param type
#'
#' @return
#' @export
#'
#' @examples
plot.run_icer_model <- function(object, type = c("regression")){
  if (type == "regression"){
    p1 <-autoplot(object[[1]], which = c(1:3, 5), label.size = 1)
    p2 <-autoplot(object[[2]], which = c(1:3, 5), label.size = 1)
    first_graph_cost <- p1[[1]] +labs(title = "COST", subtitle = "Residuals vs. Fitted") + theme(plot.title = element_text(size = 18),
                                                                                                 plot.subtitle = element_text(size = 14))
    first_graph_effect <- p2[[1]] +labs(title = "EFFECT", subtitle = "Residuals vs. Fitted") + theme(plot.title = element_text(size = 18),
                                                                                                     plot.subtitle = element_text(size = 14))
    plot_grid(first_graph_cost, first_graph_effect, p1[[2]], p2[[2]],p1[[3]], p2[[3]],p1[[4]], p2[[4]],ncol = 2)

    }
}
