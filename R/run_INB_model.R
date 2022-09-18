#' Run Incremental Net Benefit Model
#'
#'
#'
#' @param covariates a [define_covariates()] object
#' @param nb_value a [define_nb()] object
#'
#' @return
#' @export
#'
#' @examples
run_INB_model <- function(nb_value, covariates = NA) {
  # data for the model
  data <- nb_value
  data <- left_join(data, covariates, by = c("id", "tx"))
  data_lm <- data %>% select(-c(id, cost, effect))
  model <- lm(nb_value ~ tx + ., data = data_lm)

  structure(
    list(data = data,
         model = model
    ),
    class("run_inb_model")
  )
}

#' Plot INB model
#'
#' @param object a [run_INB_model()] object
#' @param type type of graph to plot, see more details
#'
#' @return
#' @export
#'
#' @examples
plot.run_INB_model <- function (object, type = c("regression", "barchart", "boxplot")){
  if (type == "regression"){
    autoplot(object[[2]], which = 1:6, label.size = 3)
  }
  else if (type == "barchart"){
    object[[1]] %>% group_by(tx) %>% summarize(average = mean(nb_value, na.rm=TRUE)) %>% ggplot(aes(fill = tx, y = average, x = tx)) + geom_bar(position="dodge", stat="identity") + labs(fill = "") +  ylab("Average Net Benefit") + ggtitle("Average Net Benefit for Each Group")
  }
  else if (type == "boxplot"){
    object[[1]] %>% ggplot(aes(x = tx, y = nb_value)) + geom_boxplot() + ggtitle("Distribution For Each Treatment Group") + xlab("") +ylab("Net Benefit")
  }

}

