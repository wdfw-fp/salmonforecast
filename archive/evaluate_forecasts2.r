#function to evaluate forecast skill ()
evaluate_forecasts2<-function(forecasts,observations){
  forecast_skill<-forecasts%>%
    # left_join(observations,by=c("Year","runsize_obs"))%>%
    dplyr::select(year,model,abundance,predicted_abundance)%>%
    mutate(error=predicted_abundance-abundance)%>%
    filter(!is.na(error))%>%
    group_by(model)%>%
    summarise(MAPE = mean(abs(error/abundance))*100,
              RMSE = sqrt(mean(error^2)),
              MSA = 100*(exp(mean(abs(log(abundance/predicted_abundance))))-1)
    )%>%
    arrange(MAPE)
  return(forecast_skill)
}
