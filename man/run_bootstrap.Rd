% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_bootstrap.R
\name{run_bootstrap}
\alias{run_bootstrap}
\title{Run bootstrap}
\usage{
run_bootstrap(
  n = 1000,
  cost,
  effect,
  lambda_min = 0,
  lambda_max = 10000,
  breaks = 5
)
}
\arguments{
\item{n}{number of repetitions for bootstrapping, default = 1000}

\item{cost}{a \code{\link[=define_cost]{define_cost()}} object}

\item{effect}{a \code{\link[=define_effect]{define_effect()}} object}

\item{lambda_min}{minimum WTP threshold to calculate probability CE}

\item{lambda_max}{maximum WTP threshold to calculate probability CE}

\item{breaks}{number of breaks between minimum and maximum WTP, or custom vector}
}
\value{
a dataframe of bootstrapped results
}
\description{
Non-parametric uncertainty analysis: bootstrapping. This function will sample the ICER estimate n times.
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
