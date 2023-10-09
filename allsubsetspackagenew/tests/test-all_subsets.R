# tests/test-all_subsets.R
library(dplyr)
library(testthat)
library(allsubsetspackagenew)
library(forecast)


# Construct the full path to the data file
data_file <- system.file("data", "dat.csv", package = "allsubsetspackagenew")

# Check if the file exists
if (!file.exists(data_file)) {
  stop("Data file 'dat.csv' not found in the package's data directory.")
}

# Read the CSV file
data <- readr::read_csv(data_file)



test_that("Test all_subsets function", {
  # Load test data from a CSV file
  test_data <- data
  # Run the function using the loaded data
  results <- all_subsets(
    series = test_data,
    covariates = covariates<-c(#new
      "lag1_log_JackOPI"
      ,"lag1_log_SmAdj"
      ,"lag1_NPGO"
      ,"lag1_PDO"
      ,"WSST_A"

      #OCN
      ,"PDO.MJJ"
      ,"MEI.OND"
      ,"UWI.JAS"
      ,"SST.AMJ"
      ,"SSH.AMJ"
      ,"UWI.SON"

      #Very correlated with variables already included
      #,"lag1_fall_Nino3.4"
      #,"SST.J"

      #Original OPIT indicators very correlated with variables already used(but untransformed)
      #"lag1_JackOPI"
      #,"lag1_SmAdj"

      #NOAA Ocean Indicators
      #,"lag1_sp_phys_trans"
      #,"lag1_PC1"
    ),
    min<-0,
    max<-1,
    type = "preseason",
    fit = TRUE
  )

  # Add test assertions
  expect_equal(length(results), 2)  # We expect two results because min=1 and max=2

  # add more test cases and assertions as needed
})
