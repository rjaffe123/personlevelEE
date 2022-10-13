#' Person Level Health Economic Modelling
#'
#' For the use of economic evaluations using person - level data
#' (clinical RCTs, health administrative, etc). Includes cost effectiveness
#' regression modelling, net benefit approaches, and both parametric and non parametric uncertainty analyses.
#'
#' @docType package
#' @name personlevelEE
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr as_tibble
#' @importFrom dplyr tibble
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr "%>%"
#' @importFrom dplyr desc
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_if
#' @importFrom dplyr funs
#' @importFrom dplyr anti_join
#'
#' @importFrom plyr ldply
#' @importFrom plyr ddply
#'
#' @importFrom lazyeval lazy
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval as.lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval interp
#'
#' @importFrom pryr standardise_call
#'
#' @importFrom utils head
#' @importFrom utils modifyList
#' @importFrom utils globalVariables
#' @importFrom utils as.roman
#'
#' @importFrom stats pnorm
#' @importFrom stats dist
#' @importFrom stats qbeta
#' @importFrom stats qbinom
#' @importFrom stats qgamma
#' @importFrom stats qlnorm
#' @importFrom stats qnorm
#' @importFrom stats terms
#' @importFrom stats setNames
#' @importFrom stats reorder
#' @importFrom stats na.omit
#' @importFrom stats update
#' @importFrom stats as.formula
#' @importFrom stats var
#' @importFrom stats coef
#' @importFrom stats model.matrix
#' @importFrom stats formula
#' @importFrom stats stepfun
#'
#' @importFrom mvnfast rmvn
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 "%+replace%"
#'
#' @importFrom memoise memoise
#' @importFrom memoise timeout
#'
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom utils packageVersion
#'
#' @importFrom tools file_ext
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices cairo_pdf
#' @importFrom grDevices png
#'
#' @importFrom graphics plot
#' @importFrom graphics par
#'
#' @importFrom tibble tibble
#'
#' @importFrom rlang sym syms quo .data
#'
#' @importFrom purrr map map_chr map_dbl map_lgl
NULL
