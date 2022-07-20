#' Define Cost and Cost Difference
#'
#' The function will store values for cost, individual ID, and treatment to be used in other functions. Will evaluate the treatment difference
#' based on values given.
#'
#' A simple linear regression will evaluate the difference in effect between treatment status.
#'
#' @param cost Vector of effect values
#' @param id Vector of individual ID
#' @param tx Vector referring to the different treatment value associated with each individual ID (should be 2 different values)
#'
#' @return A [lm()] object
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
  data_effect <- data.frame(cost = cost, id = id, tx = tx)
  final_model <- lm(cost~tx, data = data_effect)

}
