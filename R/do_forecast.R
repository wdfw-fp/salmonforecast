#function to do forecast and return plots

do_forecast<-function(
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
      ,"UWI.SON"),

    #add other inputs here (e.g., min_var, max_var, etc.)

  ){

  #find all subsets
  best_covariates<-all_subsets(series=dat,covariates=covariates,min=min_vars,max=max_vars,type=forecast_type,fit=FALSE)

  #generate one step ahead forecasts
  results<-SalmonForecasting::one_step_ahead(series=dat,
                                             leave_yrs=leave_yrs,
                                             TY_ensemble=TY_ensemble,
                                             covariates=
                                               best_covariates[[1]],
                                             first_forecast_period = first_forecast_period,
                                             plot_results = plot_results,
                                             write_model_summaries = write_model_summaries,
                                             forecast_period_start_m =  forecast_period_start_m, #inclusive
                                             forecast_period_start_d =  forecast_period_start_d, #inclusive
                                             stack_metric = stack_metric,
                                             k=k
  )


  ## not sure if we need this? if not, get rid of it, if so, let's wrap it into one of the existing function (e.g., one_step_ahead)
  model_list<-lapply(best_covariates[[1]],#[best_covariates[[2]]$model_num[1:num_models]],
                     function(x) paste(x,collapse = " + "))%>%
    unlist()%>%
    dplyr::as_tibble()%>%
    dplyr::add_rownames()%>%
    dplyr::rename(model=rowname,model_name=value)

  # rolling performance / ensemble
  rp<-SalmonForecasting::rolling_perf(one_aheads=results,
                                      series=dat,
                                      roll_years = rolling_year_window,
                                      mod_include = 10,
                                      TY_ensemble = TY_ensemble,
                                      model_list = model_list)


  ens2<-SalmonForecasting::ensemble(forecasts=(rp$all_mods %<>% dplyr::group_by(year) %>% dplyr::mutate(rank=rank(MAPE)) %>% dplyr::ungroup()),
                                    series=dat,
                                    TY_ensemble=TY_ensemble,
                                    k=k,
                                    slide=15,
                                    num_models=10,
                                    stack_metric="MAPE")
  #plot and table
 plots_and_tables<- plot_table(
    #add inputs from above
  )

  return (best_covariates=best_covariates,
          results=results,
          rp=rp,
          ens2=ens2,
          plots_and_tables=plots_and_tables
          )
}
