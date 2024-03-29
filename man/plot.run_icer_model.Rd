% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_icer_model.R
\name{plot.run_icer_model}
\alias{plot.run_icer_model}
\title{Plot ICER regressions}
\usage{
\method{plot}{run_icer_model}(x, type = c("regression", "ce-plane"), bw = FALSE, ...)
}
\arguments{
\item{x}{an \code{\link[=run_icer_model]{run_icer_model()}} object}

\item{type}{type of graph, default = regression diagnostics}

\item{bw}{Black & white plot theme for publications}

\item{...}{additional arguments affecting the plot
produced.}
}
\value{
a \code{\link[=ggplot]{ggplot()}} object
}
\description{
Plot ICER regressions
}
