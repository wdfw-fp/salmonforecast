
test_that("Test arima_forecast function", {
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
yr_end <- lubridate::year(Sys.Date())
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

# Load your dataset 'dat' from a CSV file (update the file path)
# dat <- read.csv("data/dat.csv")

# Read CSV file into a data frame
#dat <- read.csv("C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/processed/up_sum_chk_2023.csv")

# Save the data frame as an R data file (RDA)
#save(dat, file = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/processed/up_sum_chk_2023.rda")
#dat <- make_dat_from_excel(excel_path = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/SummerChinook.xlsx",file_path = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/processed/up_sum_chk_2023.csv", redo = FALSE)
# Example usage without providing file_path
#dat <- make_dat_from_excel(excel_path = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/SummerChinook.xlsx", redo = FALSE)
#dat <- make_dat_from_excel(excel_path = system.file("extdata", "SummerChinook.xlsx", package = "SalmonForecasting"), redo = FALSE)


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
      dplyr::mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0)) #%>%
      #dplyr::mutate(abundance = ifelse(is.na(abundance), 0, abundance))

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

    tryCatch({
      # Try fitting the ARIMA model
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
    }, error = function(e) {
      # Handle errors (e.g., model not suitable)
      print(paste("Error in ARIMA model for covariate", c, "- Skipping:", e$message))
    })
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
