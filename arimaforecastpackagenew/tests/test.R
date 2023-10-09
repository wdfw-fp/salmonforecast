# Load required libraries
library(forecast)
library(dplyr)
library(testthat)
library(lubridate)
library(doParallel)
library(foreach)
library(tidyverse)
library(conflicted)

# Specify preference for dplyr::filter
conflicted::conflicts_prefer(dplyr::filter)

test_that("Test arima_forecast function", {
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
  dat <- read.csv("data/dat.csv")

  series <- dat

  series<-series%>%
    ungroup()%>%
    dplyr::select(year,species,period,abundance,all_of(unique(unlist(covariates))))%>%
    filter(
      if_any(
        .cols = all_of(unique(unlist(covariates))),
        ~ !is.na(.x)
      )
    )

  exists1 =ifelse(is.na(series%>%dplyr::select(abundance)%>%tail(n=1)%>%pull()),1,0)
  cl <- makeCluster(parallel::detectCores()-3)
  registerDoParallel(cl)


  # Create a directory to store the CSV files
  dir.create("temp_list_csv", showWarnings = FALSE)

  forecasts_out <- foreach::foreach(i = 1:leave_yrs, .combine = 'rbind', .packages = c("tidyverse", "forecast")) %dopar% {
    source("functions/arima_forecast.r")

    temp_list <- list()  # Create a list to store temp objects

    for (c in 1:length(covariates)) {
      last_train_yr <- max(series$year) - (leave_yrs - i + exists1)
      tdat <- series %>%
        filter(if_any(.cols = c(year), ~ . <= (last_train_yr + 1))) %>%
        mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0))

      xreg <- tdat %>%
        filter(train_test == 0) %>%
        ungroup %>%
        dplyr::select(all_of(covariates[[c]])) %>%
        as.matrix()

      xreg_pred <- tdat %>%
        filter(train_test == 1) %>%
        ungroup %>%
        dplyr::select(all_of(covariates[[c]])) %>%
        as.matrix()

      temp <- NULL
      temp <- arima_forecast(tdat, xreg, xreg_pred, last_train_yr, first_forecast_period)
      pred <- temp$pred %>% tail(1)
      CI <- temp$CI

      tdat <- tdat %>%
        filter(train_test == 1) %>%
        dplyr::select(c("year", "period")) %>%
        bind_cols(data.frame(
          predicted_abundance = pred,
          arma = temp$arma,
          aicc = temp$aicc)) %>%
        left_join(CI, by = c("year", "period")) %>%
        mutate(model = as.character(c))

      if (c == 1) {
        forecasts <- tdat
      } else {
        forecasts <- forecasts %>% bind_rows(tdat)
      }

      # Store temp in the list
      temp_list[[c]] <- temp

      # Write temp_list to a CSV file
      temp_list_file <- file.path("temp_list_csv", paste0("temp_list_", i, "_", c, ".csv"))
      write.csv(temp_list, temp_list_file)
    }

    # Return the list of temp objects
    return(temp_list)
  }
})
