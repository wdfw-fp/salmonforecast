data <- read.csv("data/dat.csv")

test_that("Test all_subsets function", {

  library(dplyr)
  library(testthat)
  library(forecast)

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
