test_that("Test one_step_ahead function", {

  library(lubridate)
  library(dplyr)
  library(forecast)
  library(doParallel)
  library(stats)
  library(base)
  library(testthat)


  # Define the input parameters for testing
  yr_start <- 1967
  yr_end <- year(Sys.Date())
  species <- "Coho"
  date_start_analysis <- ymd("1967/1/1")
  date_end_analysis <- ymd("2023/12/31")
  forecast_period_start_m <- 1
  forecast_period_start_d <- 1
  leave_yrs <- 31
  TY_ensemble <- 16
  k <- 1
  covariates <- c(
    "lag1_log_JackOPI",
    "lag1_log_SmAdj",
    "lag1_NPGO",
    "lag1_PDO",
    "WSST_A",
    "PDO.MJJ",
    "MEI.OND",
    "UWI.JAS",
    "SST.AMJ",
    "SSH.AMJ",
    "UWI.SON"
  )
  plot_results <- FALSE
  first_forecast_period <- 1
  write_model_summaries <- TRUE
  find_best <- TRUE
  min_vars <- 0
  max_vars <- 1
  forecast_type <- "preseason"
  stack_metric <- "MAPE"
  num_models <- 10
  rolling_year_window <- 15

  # Load your dataset 'dat' from a CSV file (update the file path)
  dat <- read.csv("data/dat.csv")  # Replace with the actual file path and name

  # Call the 'one_step_ahead' function with the defined input parameters
  # Ensure that 'best_covariates' is appropriately defined in your code
  results <- one_step_ahead(series = dat,
                            leave_yrs = leave_yrs,
                            TY_ensemble = TY_ensemble,
                            covariates = covariates,  # Uncomment and adjust as needed
                            first_forecast_period = first_forecast_period,
                            plot_results = plot_results,
                            write_model_summaries = write_model_summaries,
                            forecast_period_start_m = forecast_period_start_m, # inclusive
                            forecast_period_start_d = forecast_period_start_d, # inclusive
                            stack_metric = stack_metric,
                            k = k
  )

  # Use 'expect' functions to check the structure and content of 'results'
  expect_true(is.data.frame(results))
  expect_gt(nrow(results), 0)
  # Add more expectations as needed to check specific aspects of the results

  write.csv(results, "tests/outputs/forecasts_one_step_ahead.csv")
  saveRDS(results, file = "tests/outputs/forecasts_one_step_ahead.rds")

})
