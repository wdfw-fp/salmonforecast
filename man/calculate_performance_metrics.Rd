% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_performance_metrics.R
\name{calculate_performance_metrics}
\alias{calculate_performance_metrics}
\title{Calculate performance metrics}
\usage{
calculate_performance_metrics(predicted, observed, weighted = FALSE)
}
\arguments{
\item{predicted}{Predicted values.}

\item{observed}{Observed values.}

\item{weighted}{Logical, indicating whether to calculate weighted metrics.}
}
\value{
A data frame with calculated performance metrics.
}
\description{
This function calculates performance metrics (MAPE, RMSE, MSA, MASE) for a given set of predictions and observations.
}
