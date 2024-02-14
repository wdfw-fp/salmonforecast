#' Perform Forecasting with Ensemble Models
#'
#' This function conducts time series forecasting using ensemble models.
#' @name do_forecast
#' @title Perform Forecasting with Ensemble Models
#' @param covariates A vector specifying the covariates to be considered in the forecasting models.
#' @param leave_yrs Number of years to leave out for model validation.
#' @param TY_ensemble Number of models in the ensemble.
#' @param first_forecast_period The starting period for making forecasts.
#' @param plot_results Logical, indicating whether to plot forecast results.
#' @param write_model_summaries Logical, indicating whether to write model summaries.
#' @param forecast_period_start_m Starting month of the forecast period (inclusive).
#' @param forecast_period_start_d Starting day of the forecast period (inclusive).
#' @param stack_metric The metric used for stacking models in the ensemble.
#' @param k Number of top models to include in the ensemble.
#' @param min_vars Minimum number of covariates in a model.
#' @param max_vars Maximum number of covariates in a model.
#' @param forecast_type Type of forecasting approach ("preseason" or other).
#' @param rolling_year_window Number of years for rolling performance calculation.
#' @param num_models Number of top models to consider in ensemble creation.
#'
#' @return A list containing tables and plots summarizing the forecasting results and a list containing various outputs, including selected covariates, forecast results, rolling performance, ensemble models, and plots/tables.
#'
#' @importFrom dplyr mutate group_by %>%
#' @import ggplot2
#' @export
#'

#function to do forecast and return plots

do_forecast<-function(
    covariates=c(#new
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

    leave_yrs=31,
    TY_ensemble=16,
    first_forecast_period = 1,
    plot_results = FALSE,
    write_model_summaries = TRUE,
    forecast_period_start_m =  1, #inclusive
    forecast_period_start_d =  1, #inclusive
    stack_metric = "MAPE",
    k=1,
    min_vars=0,
    max_vars=1,
    forecast_type="preseason",
    rolling_year_window=15,
    num_models=10


){

  #find all subsets
  best_covariates<-all_subsets(series=dat,covariates=covariates,min=min_vars,max=max_vars,type=forecast_type,fit=FALSE)

  #generate one step ahead forecasts
  results<-one_step_ahead(series=dat,
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
  rp<-rolling_perf(one_aheads=results,
                   series=dat,
                   roll_years = rolling_year_window,
                   mod_include = 10,
                   TY_ensemble = TY_ensemble,
                   model_list = model_list)


  ens<-ensemble(forecasts=(rp$all_mods %<>% dplyr::group_by(year) %>% dplyr::mutate(rank=rank(MAPE)) %>% dplyr::ungroup()),
                series=dat,
                TY_ensemble=TY_ensemble,
                k=k,
                slide=15,
                num_models=num_models,
                stack_metric="MAPE")
  #plot and table
  plots_and_tables<- plot_table(
    rp=rp,
    ens=ens,
    stack_metric=stack_metric,
    rolling_year_window=rolling_year_window
  )
  #return = plots_and_tables
  return(list(
    best_covariates = best_covariates,
    results = results,
    rp = rp,
    plots_and_tables = plots_and_tables
  ))

}
