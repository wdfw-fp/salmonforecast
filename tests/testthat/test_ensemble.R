
test_that("Test ensemble function", {
  set.seed(123)
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
  #=========
  # Raw Data
  #=========
  yr_start<-1967
  yr_end<-year(Sys.Date())
  #dam="BON"
  species="Coho"
  #===========================
  # Summarization for analysis
  #===========================
  date_start_analysis <- lubridate::ymd("1967/1/1")
  date_end_analysis <- lubridate::ymd("2023/12/31")
  forecast_period_start_m<-1 #this will be the month associated with the first month/day of the seasonal estimate period each year...can be after first data used to estimate it or the same
  forecast_period_start_d<-1 #this will be the month day associated with the first month/day of the seasonal estimate period each year...can be after first data used to estimate it or the same
  last_data<-Sys.Date()
  #==================
  #forecasting params
  #==================
  leave_yrs<- 31
  TY_ensemble<-16
  k<-1
  covariates<-covariates
  plot_results = F
  first_forecast_period = 1
  write_model_summaries = TRUE
  find_best=T
  #==============
  #Ensemble Params
  #===============
  min_vars<-0
  max_vars<-1
  forecast_type<-"preseason"
  stack_metric<-"MAPE"
  num_models<-3
  rolling_year_window<-15

  inputs<-list(
    yr_start = yr_start,
    yr_end = yr_end,
    species = species,
    date_start_analysis = date_start_analysis,
    date_end_analysis = date_end_analysis,
    forecast_period_start_m = forecast_period_start_m,
    forecast_period_start_d = forecast_period_start_d,
    last_data = last_data,
    leave_yrs = leave_yrs,
    TY_ensemble = TY_ensemble,
    k = k,
    covariates = covariates,
    plot_results = plot_results,
    first_forecast_period = first_forecast_period,
    write_model_summaries = write_model_summaries,
    find_best = find_best,
    min_vars = min_vars,
    max_vars = max_vars,
    forecast_type = forecast_type,
    stack_metric = stack_metric,
    num_models = num_models,
    rolling_year_window
  )



  dat<-dat


  if(find_best ==T){
    best_covariates<-all_subsets(series=dat,covariates=covariates,min=min_vars,max=max_vars,type=forecast_type,fit=FALSE)
    saveRDS(best_covariates,"outputs/best_covariates.rds")
  }

  best_covariates<-readRDS("outputs/best_covariates.rds")


  model_list <- lapply(best_covariates[[1]], function(x) paste(x, collapse = " + ")) %>%
    unlist() %>%
    as_tibble() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(model = rowname, model_name = value)


  fit_one_step<-F
  if(fit_one_step){


    results<-one_step_ahead(series=dat,
                            leave_yrs=leave_yrs,
                            TY_ensemble=TY_ensemble,
                            # covariates= best_covariates[[1]][best_covariates[[2]]$model_num[1:num_models]],
                            best_covariates[[1]],
                            first_forecast_period = first_forecast_period,
                            plot_results = plot_results,
                            write_model_summaries = write_model_summaries,
                            forecast_period_start_m =  forecast_period_start_m, #inclusive
                            forecast_period_start_d =  forecast_period_start_d, #inclusive
                            stack_metric = stack_metric,
                            k=k
    )

    write.csv(results,"outputs/forecasts.csv")

    save(results,file="outputs/forecasts.rds")
  }else{
    results<-read_csv("forecasts.csv")
  }


  rp<-rolling_perf(results,dat,rolling_year_window,3,TY_ensemble,model_list)

  # Call ensemble function
  ens <- ensemble(
    forecasts=rp$all_mods %>% group_by(year) %>% mutate(rank=rank(MAPE)) %>% ungroup,
    series = dat,
    TY_ensemble = TY_ensemble,
    k = k,
    slide = 15,
    num_models=num_models,
    stack_metric=stack_metric
  )




  # Ensure consistent data types for numeric columns in final_model_weights
  final_model_weights$RMSE <- as.numeric(final_model_weights$RMSE)
  final_model_weights$MAPE <- as.numeric(final_model_weights$MAPE)

  # Check for duplicates in row names
  if (anyDuplicated(row.names(final_model_weights))) {
    stop("Duplicate row names found in final_model_weights.")
  }

  # Check for missing values in row names
  if (anyNA(row.names(final_model_weights))) {
    stop("Missing row names found in final_model_weights.")
  }

  # Fix variable name inconsistency (Stack_weighted vs Stacking_weight)
  final_model_weights$Stack_weighted <- final_model_weights$Stacking_weight
  final_model_weights <- final_model_weights[, !grepl("Stacking_weight", names(final_model_weights))]

  # Assuming 'ens' is your data frame
  expect_true(!is.null(ens))

  # Print the first 10 rows of 'ens'
  print(head(ens, n = 10))

})
