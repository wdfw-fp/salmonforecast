% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_forecast.R
\name{do_forecast}
\alias{do_forecast}
\title{Perform Forecasting with Ensemble Models}
\usage{
do_forecast(
  dat,
  covariates = c("lag1_log_JackOPI", "lag1_log_SmAdj", "lag1_NPGO", "lag1_PDO", "WSST_A",
    "PDO.MJJ", "MEI.OND", "UWI.JAS", "SST.AMJ", "SSH.AMJ", "UWI.SON"),
  TY_ensemble = 16,
  slide = 15,
  first_forecast_period = 1,
  plot_results = FALSE,
  write_model_summaries = TRUE,
  forecast_period_start_m = 1,
  forecast_period_start_d = 1,
  do_stacking = FALSE,
  stack_metric = "MAPE",
  k = 1,
  min_vars = 0,
  max_vars = 1,
  forecast_type = "preseason",
  screen_metric = AICc,
  num_models = 25,
  n_cores = 2,
  ts_freq = 1,
  seasonal = FALSE,
  exp_smooth_alpha = 0,
  include_mod = FALSE
)
}
\arguments{
\item{dat}{data frame with columns "year", "species", "period", "abundance", and the covariates included in the covariates argument}

\item{covariates}{A vector specifying the covariates to be considered in the forecasting models.}

\item{TY_ensemble}{number of years ti evaluate performance based on plus 1.}

\item{slide}{The length of the sliding window for calculating ensemble weights}

\item{first_forecast_period}{The starting period for making forecasts.}

\item{plot_results}{Logical, indicating whether to plot forecast results.}

\item{write_model_summaries}{Logical, indicating whether to write model summaries.}

\item{forecast_period_start_m}{Starting month of the forecast period (inclusive).}

\item{forecast_period_start_d}{Starting day of the forecast period (inclusive).}

\item{stack_metric}{The metric used for stacking models in the ensemble.}

\item{k}{Number of top models to include in the ensemble.}

\item{min_vars}{Minimum number of covariates in a model.}

\item{max_vars}{Maximum number of covariates in a model.}

\item{forecast_type}{Type of forecasting approach ("preseason" or other).}

\item{num_models}{Number of top models to consider in ensemble creation.}

\item{n_cores}{number of cores to use in parallel computing}

\item{ts_freq}{frequency for time series (generally 1)}

\item{seasonal}{whether the arima models should include seasonal  components}

\item{alpha}{the annual rate of decay per year used in weighting performance in metric  calculation for model selection and ensemble-weights}
}
\value{
A list containing tables and plots summarizing the forecasting results and a list containing various outputs, including selected covariates, forecast results, rolling performance, ensemble models, and plots/tables.
}
\description{
Perform Forecasting with Ensemble Models
}
\details{
This function conducts time series forecasting using ensemble models.
}
