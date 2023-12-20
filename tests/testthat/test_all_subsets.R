
# Test all_subsets function
test_that("Test all_subsets function", {
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
  use_rda <- FALSE
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


  # Run the function using the loaded data
  results <- all_subsets(



    series = dat,
    covariates=covariates,
    min<-0,
    max<-1,
    type = "preseason",
    fit = TRUE
  )

  # Add test assertions
  expect_equal(length(results), 2)  # We expect two results because min=1 and max=2

  # add more test cases and assertions as needed
})
