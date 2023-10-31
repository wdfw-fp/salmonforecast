#' Perform rolling performance analysis of models on time series data.
#'
#' This function calculates the rolling performance of models on time series data.
#'
#' @param one_aheads A data frame containing one-step-ahead forecasts and model information.
#' @param series A data frame containing time series data with "year" and "abundance" columns.
#' @param roll_years The number of years to use for rolling window calculations (default: 15).
#' @param mod_include The number of top-performing models to include in the analysis (default: 5).
#' @param TY_ensemble The number of years for the ensemble analysis (default: 16).
#' @param model_list
#' @return A list containing the following components:
#'   - all_mods: A data frame with rolling performance metrics for all models.
#'   - top_mods: A data frame with rolling performance metrics for the top models.
#'   - performance: A summary of the overall performance metrics.
#'
#' @examples
# \dontrun{
#   # Example usage:
#   result <- rolling_perf(one_aheads_data, series_data)
#   print(result)
# }
#'
#' @import dplyr
#' @import zoo
#'
#' @export
#'
rolling_perf<-function(one_aheads,series,roll_years,mod_include,TY_ensemble,model_list){


  out<-one_aheads  %>%left_join(series %>% dplyr::select(year,abundance)) %>%
    mutate(error=abundance-predicted_abundance,
           APE=abs(error/abundance))%>%
    arrange(model,year) %>%
    group_by(model) %>%
    mutate(MAPE= lag( 100*zoo::rollmean(APE, k = roll_years, fill = NA, align = "right"))) %>%
    group_by(year) %>%
    mutate(rank=rank(MAPE),
           model=as.character(model))


  #top performing models in each window
  tops<-out%>%
    filter(between(year,2023-TY_ensemble+1,2023),
           rank<=mod_include) %>%
    left_join(model_list) %>%
    arrange(desc(year),rank) #%>% dplyr::select(-1)



  # performance of best model
  perf<-tops%>%
    ungroup %>%
    filter(rank==1) %>%
    summarize(MAPE=mean(APE,na.rm=T)*100,
              RMSE = sqrt(mean(error^2,na.rm=T)),
              MSA = 100*(exp(mean(abs(log(abundance/predicted_abundance)),na.rm=T))-1))




  return(list(
    all_mods=out %>% ungroup(),
    top_mods=tops %>% ungroup(),
    performance=perf
  ))
}
