test_that("Test ensemble function", {
  set.seed(123)

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
  covariates<-c(#new
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
  )
  plot_results = F
  first_forecast_period = 1
  write_model_summaries = TRUE
  find_best=T
  #==============
  #Ensemble Params
  #===============
  min_vars<-0
  max_vars<-6
  forecast_type<-"preseason"
  stack_metric<-"MAPE"
  num_models<-10
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
    saveRDS(best_covariates,"outputs/best_covariates_OPI_preseason.rds")
  }

  best_covariates<-readRDS("outputs/best_covariates_OPI_preseason.rds")


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


  expect_true(!is.null(ens))

  # Assuming 'ens' is your data frame
  print(head(ens, n = 10))

})
