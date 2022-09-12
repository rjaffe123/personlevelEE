#' Define net benefit (NB) values
#'
#' Creates a smaller
#'
#'
#' @param lambda a value for WTP threshold
#' @param cost a [define_cost()] object
#' @param effect a [define_effect()] object
#'
#' @return return dataframe that includes NB values associated with an individual id based on given lambda
#' @export
#'
#' @examples
define_NB <- function(lambda, cost, effect) {
  data <- left_join(cost[[1]], effect[[1]], by = c("id", "tx"))
  data <- data %>% mutate(nb = lambda*effect - cost)
  return(data)
}
