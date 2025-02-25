test_that("Test do_forecast function", {

  set.seed(123)
  dat=dat
  output=do_forecast   ( covariates=c(#new
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

    dat=dat,
    TY_ensemble=16,
    #min_ens_yrs=3,
    first_forecast_period = 1,
    plot_results = FALSE,
    write_model_summaries = TRUE,
    forecast_period_start_m =  1,
    forecast_period_start_d =  1,
    stack_metric = "MAPE",
    k=1,
    min_vars=0,
    max_vars=1,
    forecast_type="preseason",
    num_models=10,
    n_cores=2


  )

  # Extract the plots_and_tables element
  plots_and_tables_output <- output$plots_and_tables
  # Check if results is equal to plots_and_tables, ignoring structure
  expect_true(!is.null(output))


})
