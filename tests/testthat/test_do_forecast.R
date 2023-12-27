test_that("Test do_forecast function", {

  set.seed(123)
  dat=dat
  results=do_forecast   ( covariates=c(#new
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


)
  expect_true(!is.null(results))



})
