#' @name one_step_ahead
#' @title Function to evaluate the performance of ARIMA model (produces season total forecasts only)
#' @description This function evaluates the performance of an ARIMA model for season total forecasts using provided covariates and other parameters.
#' @param series A data frame containing the time series data.
#' @param leave_yrs Number of years to leave out during the evaluation.
#' @param TY_ensemble A parameter for ensemble evaluation.
#' @param covariates A list of covariates to include in the model.
#' @param first_forecast_period The first period for forecasting.
#' @param plot_results A logical value to indicate whether to plot results.
#' @param write_model_summaries A logical value to indicate whether to write model summaries.
#' @param forecast_period_start_m The starting month for the forecast period (inclusive).
#' @param forecast_period_start_d The starting day for the forecast period (inclusive).
#' @param stack_metric A metric used for stacking models.
#' @param k A parameter for the model evaluation.
#'
#' @return A data frame containing forecasts and other information.
#'
#' @export
#'
#' @import doParallel
#' @importFrom dplyr select filter mutate ungroup pull bind_cols left_join rename bind_rows %>%
#' @importFrom utils write.table tail
#' @import forecast
# Function to evaluate the performance of ARIMA model (produces season total forecasts only)
# one_step_ahead <- function(series,
#                            leave_yrs,
#                            TY_ensemble,
#                            covariates,
#                            first_forecast_period,
#                            plot_results,
#                            write_model_summaries,
#                            forecast_period_start_m, #inclusive
#                            forecast_period_start_d, #inclusive
#                            obs_period_2,
#                            p1_covariates_only,
#                            stack_metric,
#                            k
# ) {
#   start <- Sys.time()
#
#   if (write_model_summaries == TRUE) {
#     file.create("summary.txt")
#
#     #base::write.table(NULL, "summary.txt")
#   }
#
#   series <- series %>%
#     dplyr::ungroup() %>%
#     dplyr::select(year, species, period, abundance, !!!syms(covariates)) %>%
#     dplyr::filter(
#       dplyr::across(
#         .cols = !!!syms(covariates),
#         .fns = ~ !dplyr::is.na(.x)
#       )
#     )
#   exists1 <- ifelse(dplyr::is.na(series %>% dplyr::select(abundance) %>% dplyr::tail(n = 1) %>% dplyr::pull()), 1, 0)
#
#   parallel::library(doParallel)
#   cl <- parallel::makeCluster(parallel::detectCores() - 3)
#   parallel::registerDoParallel(cl)
#   forecasts_out <- foreach::foreach(i = 1:leave_yrs, .combine = 'rbind', .packages = "forecast") %dopar% {
#     for (c in 1:length(covariates)) {
#       # for(i in 1:leave_yrs){
#       last_train_yr <- max(series$year) - (leave_yrs - i + exists1)
#       tdat <- series %>%
#         dplyr::filter(year <= (last_train_yr + 1)) %>%
#         dplyr::mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0))
#
#       xreg <- tdat %>%
#         dplyr::filter(train_test == 0) %>%
#         dplyr::ungroup() %>%
#         dplyr::select(all_of(covariates[[c]])) %>%
#         dplyr::as.matrix()
#
#       xreg_pred <- tdat %>%
#         dplyr::filter(train_test == 1) %>%
#         dplyr::ungroup() %>%
#         dplyr::select(all_of(covariates[[c]])) %>%
#         dplyr::as.matrix()
#
#       temp <- NULL
#       temp <- forecast::arima_forecast(tdat, xreg, xreg_pred, last_train_yr, first_forecast_period)
#       pred <- temp$pred %>% dplyr::tail(1)
#       CI <- temp$CI
#
#       tdat <- tdat %>% dplyr::filter(train_test == 1) %>% dplyr::select(c("year", "period")) %>%
#         dplyr::bind_cols(data.frame(
#           predicted_abundance = pred,
#           arma = temp$arma,
#           aicc = temp$aicc)) %>%
#         dplyr::left_join(CI, by = c("year", "period")) %>%
#         dplyr::mutate(model = dplyr::as.character(c))
#
#       if (c == 1) {
#         forecasts <- tdat
#       } else {
#         forecasts <- forecasts %>% dplyr::bind_rows(tdat)
#       }
#     }
#     return(forecasts)
#   }
#
#   # ...
#
#   parallel::stopCluster(cl)
#
#   print((Sys.time() - start))
#   return(forecasts_out)
# }
# Define the one_step_ahead function with the corrections
one_step_ahead <- function(series,
                           leave_yrs,
                           TY_ensemble,
                           covariates,
                           first_forecast_period,
                           plot_results,
                           write_model_summaries,
                           forecast_period_start_m, # inclusive
                           forecast_period_start_d, # inclusive
                           stack_metric,
                           k
) {
  species <- "Coho"
  abundance <- series$abundance
  train_test<-series$train_test

  start <- Sys.time()

  if (write_model_summaries == TRUE) {
    write.table(NULL, "summary.txt")
  }

  # Filter the series based on covariates outside of the if_any
  # filtered_series <- series %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(year, species, period, abundance, all_of(covariates))





  filtered_series<-series%>%
    ungroup()%>%
    dplyr::select(year,species,period,abundance,all_of(unique(unlist(covariates))))%>%
    filter(
      dplyr::across(
        .cols = all_of(unique(unlist(covariates))),
        .fns = ~ !is.na(.x)
      )
    )

  # Determine if any covariate columns have NA values
  #has_na_covariates <- colSums(is.na(filtered_series[, covariates])) > 0

  #cl <- makeCluster(parallel::detectCores() - 3)
  cl <- makeCluster(2)  # Use 2 cores as an example, adjust as needed
  registerDoParallel(cl)
  forecasts_out <- foreach::foreach(i = 1:leave_yrs, .combine = 'rbind', .packages = c("forecast")) %dopar% {
    i <- i  # Define 'i' within the foreach loop
    for (c in 1:length(covariates)) {
      last_train_yr <- max(series$year) - (leave_yrs - i + 1)
      tdat <- filtered_series %>%
        filter(year <= (last_train_yr + 1)) %>%
        mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0)
        )

      xreg <- tdat %>%
        filter(train_test == 0) %>%
        ungroup() %>%
        dplyr::select(all_of(covariates[[c]])) %>%
        as.matrix()

      xreg_pred <- tdat %>%
        filter(train_test == 1) %>%
        ungroup() %>%
        dplyr::select(all_of(covariates[[c]])) %>%
        as.matrix()

      temp <- NULL
      temp <- arima_forecast(tdat, xreg, xreg_pred, last_train_yr, first_forecast_period)
      pred <- temp$pred %>% tail(1)
      CI <- temp$CI

      tdat <- tdat %>% filter(train_test == 1) %>% dplyr::select(c("year", "period")) %>%
        bind_cols(data.frame(
          predicted_abundance = pred,
          arma = temp$arma,
          aicc = temp$aicc)) %>%
        left_join(CI, by = c("year", "period"))

      if (c == 1) {
        forecasts <- tdat
      } else {
        forecasts <- dplyr::bind_rows(forecasts, tdat)
      }
    }
    return(forecasts)
  }

  stopCluster(cl)

  print((Sys.time() - start))
  return(forecasts_out)
}
