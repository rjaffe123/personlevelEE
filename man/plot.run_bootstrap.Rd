% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_bootstrap.R
\name{plot.run_bootstrap}
\alias{plot.run_bootstrap}
\title{Plot output of bootstrap}
\usage{
\method{plot}{run_bootstrap}(x, type = c("cloud", "ceac"), bw = FALSE, ...)
}
\arguments{
\item{x}{object of \code{\link[=run_bootstrap]{run_bootstrap()}}}

\item{type}{type of graph to show, either "cloud" or "ceac"}

\item{bw}{black & white plot theme?}

\item{...}{additional arguments affecting the plot
produced.}
}
\value{
a \code{\link[=ggplot2]{ggplot2()}} object
}
\description{
Plot output of bootstrap
}
\examples{
library(personlevelEE)

data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example, cost = cost, id = id, tx = tx)

effect_object <- define_effect(data_example, effect = effect, id = id, tx = tx)

bootstrap <- run_bootstrap(n=1000, cost = cost_object,
                           effect = effect_object, lambda_min = 10,
                           lambda_max = 10000, breaks = 4)

plot(bootstrap, type = "cloud", bw =TRUE)
plot(bootstrap, type = "ceac", bw = TRUE)
}
