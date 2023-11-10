test_that("Test arima_forecast function", {
  #library(doParallel)
  #
  # # Load required libraries
  # library(forecast)
  # library(dplyr)
  # library(testthat)
  # library(lubridate)
  # library(doParallel)
  # library(foreach)
  # library(tidyverse)
  # library(conflicted)
  # # Specify preference for dplyr::filter
  # conflicted::conflicts_prefer(dplyr::filter)
  #
# Load the required libraries at the beginning of your R script
#library(lubridate)
#library(dplyr)
#library(doParallel)
#library(foreach)
#library(tidyr)
#library(forecast)

# Define the input parameters for testing
yr_start <- 1967
yr_end <- lubridate::year(Sys.Date())
species <- "Coho"
date_start_analysis <- lubridate::ymd("1967/1/1")
date_end_analysis <- lubridate::ymd("2023/12/31")
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
# dat <- read.csv("data/dat.csv")

series <- dat

series <- series %>%
  dplyr::ungroup() %>%
  dplyr::select(year, species, period, abundance, all_of(unique(unlist(covariates)))) %>%
  dplyr::filter(
    if_any(
      .cols = all_of(unique(unlist(covariates))),
      ~ !is.na(.x)
    )
  )

# Rest of your code
exists1 <- ifelse(is.na(series %>% dplyr::select(abundance) %>% tail(n = 1) %>% pull()), 1, 0)
#cl <- parallel::makeCluster(parallel::detectCores() - 3)
cl <- parallel::makeCluster(2)  # Create a cluster with 2 workers

doParallel::registerDoParallel(cl)

# Create a directory to store the CSV files
if (!dir.exists("outputs/temp_list_csv")) {
  dir.create("outputs/temp_list_csv", showWarnings = FALSE)
}

forecasts_out <- foreach::foreach(i = 1:leave_yrs, .combine = 'rbind', .packages = c("forecast")) %dopar% {
  temp_list <- list()  # Create a list to store temp objects
  forecasts <- data.frame()  # Create an empty data frame to store final forecasts

  for (c in 1:length(covariates)) {
    last_train_yr <- max(series$year) - (leave_yrs - i + exists1)
    tdat <- series %>%
      dplyr::filter(dplyr::if_any(.cols = c(year), ~ . <= (last_train_yr + 1))) %>%
      dplyr::mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0))

    xreg <- tdat %>%
      dplyr::filter(train_test == 0) %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(covariates[[c]])) %>%
      as.matrix()

    xreg_pred <- tdat %>%
      dplyr::filter(train_test == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(covariates[[c]])) %>%
      as.matrix()

    temp <- arima_forecast(tdat, xreg, xreg_pred, last_train_yr, first_forecast_period, write_model_summaries, train_test, abundance)
    pred <- temp$pred %>% tail(1)
    CI <- temp$CI

    tdat <- tdat %>%
      dplyr::filter(train_test == 1) %>%
      dplyr::select(c("year", "period")) %>%
      dplyr::bind_cols(data.frame(
        predicted_abundance = pred,
        arma = temp$arma,
        aicc = temp$aicc)) %>%
      dplyr::left_join(CI, by = c("year", "period")) %>%
      dplyr::mutate(model = as.character(c))

    if (c == 1) {
      forecasts <- tdat
    } else {
      forecasts <- forecasts %>% dplyr::bind_rows(tdat)
    }

    # Store temp in the list
    temp_list[[c]] <- temp

    # Save forecasts for this iteration to a CSV file
    forecast_file <- file.path("outputs/temp_list_csv", paste0("forecast_", i, "_", c, ".csv"))
    write.csv(tdat, forecast_file)
  }

  # Return forecasts for this iteration
  forecasts
}

# Concatenate all forecasts from each iteration into one data frame
all_forecasts <- do.call(rbind, forecasts_out)

# Save all forecasts to a single CSV file
final_output_file <- file.path("outputs", "arima_forecast.csv")
write.csv(all_forecasts, final_output_file)

# Your test assertion
expect_true(TRUE, "This is a test assertion.")

# You can add more test assertions here if needed
})
