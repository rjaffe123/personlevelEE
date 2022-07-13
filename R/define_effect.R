#' Define the Effect
#'
#' The function will store values for effect, individual ID, and treatment to be used in other functions. Will evaluate the treatment difference
#' based on values given.
#'
#' A simple linear regression will evaluate the difference in effect between treatment status.
#'
#' @param effect Vector of effect values
#' @param id Vector of individual ID
#' @param tx Vector referring to the different treatment value associated with each individual ID (should be 2 different values)
#'
#' @return A [lm()] object
#' @export
#'
#' @examples
define_effect <- function(effect, id, tx){
  ## errors:
  ## if effect != vector
  ## if id != vector
  ## if effect & id not same length
  ## if id values not numerical
  ## perform linear regression
  data_effect <- data.frame(effect = effect, id = id, tx = tx)
  final_model <- lm(effect~tx, data = data_effect)
}

## format of print:
## regression values
## incremental effect difference
## plot.effect <- type ("regression diagnostics", "barchart")
