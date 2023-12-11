test_that("Test all_subsets function", {
  # Function to load data from .rda file or generate it using make_dat_from_excel
  load_or_generate_data <- function(use_rda = TRUE, excel_path = NULL) {
    if (use_rda) {
      # Load data from .rda file in the "data" folder
      dat <- dat
    } else {
      # Generate data using make_dat_from_excel
      dat <- make_dat_from_excel(excel_path = excel_path, redo = FALSE)
    }

    return(dat)
  }

  # Set use_rda to TRUE to load from .rda file or FALSE to generate from Excel
  use_rda <- TRUE
  excel_path <- system.file("extdata", "SummerChinook.xlsx", package = "SalmonForecasting")

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

  # Save the data frame as an R data file (RDA)
  #save(dat, file = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/processed/up_sum_chk_2023.rda")
  #dat <- make_dat_from_excel(excel_path = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/SummerChinook.xlsx", file_path = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/processed/up_sum_chk_2023.csv", redo = FALSE)
  # Example usage without providing file_path
  #dat <- make_dat_from_excel(excel_path = "C:/Users/tjyua/OneDrive/Desktop/Data498/SalmonForecasting/data/SummerChinook.xlsx", redo = FALSE)
  #dat <- make_dat_from_excel(excel_path = system.file("extdata", "SummerChinook.xlsx", package = "SalmonForecasting"), redo = FALSE)


  # Load test data from a CSV file

  # Run the function using the loaded data
  results <- all_subsets(



    series = dat,
    covariates=covariates,






    # covariates = covariates<-c(
    #   "lag1_log_Jack",
    #   "lag4_log_adults",
    #   "lag5_log_adults",
    #   "lag1_log_SAR",
    #   "lag2_log_SAR",
    #   "lag1_NPGO",
    #   "lag1_PDO",
    #   "lag2_NPGO",
    #   "lag2_PDO",
    #   "lag2_PC1",
    #   "lag2_PC2",
    #   "lag2_sp_phys_trans",
    #   "pink_ind",
    #   "lag1_log_socksmolt"
    # ),
    min<-0,
    max<-1,
    type = "preseason",
    fit = TRUE
  )

  # Add test assertions
  expect_equal(length(results), 2)  # We expect two results because min=1 and max=2

  # add more test cases and assertions as needed
})
