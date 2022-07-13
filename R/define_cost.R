#' Define the Cost
#'
#' Define the vector used for the values to measure effect.
#'
#' Function will
#'
#' @param cost Vector of numerical values referring to cost
#' @param id Vector of numerical values referring to individual ID
#' @param tx Vector referring to the different treatment values associated with each individual ID
#'
#' @return A data frame of a cost values associated to an individual ID
#' @export
#'
#' @examples
define_cost <- function(cost, id, tx){
  ## errors:
  ## if cost != vector
  ## if id != vector
  ## if cost & id not same length
  ## if id values not numerical
  ## perform linear regression
  data_effect <- data.frame(effect = effect, id = id, tx = tx)
  final_model <- lm(effect~tx, data = data_effect)

}
