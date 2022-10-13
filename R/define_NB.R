#' Define net benefit (NB) values
#'
#' Creates the NB values for each individual based on the lambdas inputted.
#'
#'
#' @param lambdas value(s) for WTP threshold
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#'
#' @return return dataframe that includes NB values associated with an individual id based on given lambda
#' @export
#'
#' @example inst/examples/example_define_NB.R
define_NB <- function(lambdas, cost, effect) {
  new_columns <- c()
  data <- dplyr::left_join(cost$data_cost, effect$data_effect, by = c("id", "tx"))
  for (x in lambdas){
    cat("\n Calculating a net benefit value for: ", x)
    nb_values <- x*data$effect - data$cost
    new_columns <- c(new_columns, paste0("nb_", x))
    data[,paste0("nb_", x)] <- nb_values
  }
  structure(
    list(data = data,
         lambda = lambdas,
         column_names =new_columns
    ),
    class= "define_NB"
  )
}

#' Print NB data
#'
#' @param x an [define_NB()] object
#' @param ... additional arguments affecting the print.
#'
#' @export
#'
print.define_NB <- function(x, ...){
  print(head(x$data))
}

