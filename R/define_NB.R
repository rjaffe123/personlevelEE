#' Define net benefit (NB) values
#'
#' Creates a smaller
#'
#'
#' @param lambda
#' @param id
#' @param cost
#' @param effect
#'
#' @return return dataframe that includes NB values associated with an individual id based on given lambda
#' @export
#'
#' @examples
define_NB <- function(lambda, cost, effect, id) {
  data_effect <- data.frame(effect = effect, id = id, tx = tx)
  data_effect <- data_effect %>% mutate(nb = lambda*effect - cost)
}
