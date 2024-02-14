
test_that("Test one_step_ahead function", {
  load_or_generate_data <- function(use_rda = TRUE, excel_path = NULL) {
    if (use_rda) {
      # Load data from .rda file in the "data" folder
      dat <- dat
    } else {
      csv_path <- system.file("data", "up_sum_chk.csv", package = "SalmonForecasting")

      if (file.exists(csv_path)) {
        # Read data from CSV if it exists
        up_sum_chk <- read.csv(csv_path)
      } else {
        # Generate data using make_dat_from_excel
        up_sum_chk <- readxl::read_xlsx(excel_path, sheet = 1) %>%
          brood_to_return() %>%
          mutate(
            abundance = Age4 + Age5 + Age6
          ) %>%
          dplyr::select(year = ReturnYear, abundance, Age4, Jack = Age3) %>%
          arrange(year)

        #Save up_sum_chk as CSV
        write.csv(up_sum_chk, csv_path, row.names = FALSE)
      }

      dat <- make_dat(dat1 = up_sum_chk)
    }

    return(dat)
  }
  # Set use_rda to TRUE to load from .rda file or FALSE to generate from Excel
  use_rda <- TRUE
  excel_path <- system.file("data", "SummerChinook.xlsx", package = "SalmonForecasting")

  # Call the function
  dat <- load_or_generate_data(use_rda = use_rda, excel_path = excel_path)

  # Conditional assignment of covariates
  if (use_rda) {
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
  } else {
    covariates <- c(
      "lag1_log_Jack",
      "lag4_log_adults",
      "lag5_log_adults",
      "lag1_log_SAR",
      "lag2_log_SAR",
      "lag1_NPGO",
      "lag1_PDO",
      "lag2_NPGO",
      "lag2_PDO",
      "lag2_PC1",
      "lag2_PC2",
      "lag2_sp_phys_trans",
      "pink_ind",
      "lag1_log_socksmolt"
    )
  }



  # Define the input parameters for testing
  yr_start <- 1967
  yr_end <- year(Sys.Date())
  #species <- "Coho"
  date_start_analysis <- lubridate::ymd("1967/1/1")
  date_end_analysis <- lubridate::ymd("2023/12/31")
  forecast_period_start_m <- 1
  forecast_period_start_d <- 1
  leave_yrs <- 31
  TY_ensemble <- 16
  k <- 1
  covariates <- covariates
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



  best_covariates<-all_subsets(series=dat,covariates=covariates,min=min_vars,max=max_vars,type=forecast_type,fit=FALSE)


  # Call the 'one_step_ahead' function with the defined input parameters
  # Ensure that 'best_covariates' is appropriately defined in your code
  results <- one_step_ahead(series = dat,  # Replace your_package_name
                            leave_yrs = leave_yrs,
                            TY_ensemble = TY_ensemble,
                            covariates =best_covariates[[1]],  # Uncomment and adjust as needed
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

  write.csv(results, "outputs/forecasts.csv")
  saveRDS(results, file = "outputs/forecasts.rds")
})
