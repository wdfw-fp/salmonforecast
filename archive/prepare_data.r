#function to summarize data for forecasting into period sums
#dates between the last observed date of the last year and the beginning of the second period are automatically deleted so period 1 counts are comparable among years
prepare_data<-function(series,
                       date_start_analysis, #inclusive
                       date_end_analysis, #inclusive
                       forecast_period_start_m, #inclusive 
                       forecast_period_start_d, #inclusive
                       use_freshest_data,
                       covariates,
                       p1_covariates_only
){
  
  
  p_2_end_m<-month(date_end_analysis)
  p_2_end_d<-mday(date_end_analysis)
  
  series<-series%>%
    filter(date >= date_start_analysis & date <= date_end_analysis)%>%
    filter(month(date) > month(date_start_analysis) | month(date) == month(date_start_analysis) & mday(date) >= mday(date_start_analysis)) %>%
    filter(month(date) < month(date_end_analysis) | month(date) == month(date_end_analysis) & mday(date) <= mday(date_end_analysis))
    
  
  last_obs_d<-mday(max(series$date))
  last_obs_m<-month(max(series$date))
  
  if(use_freshest_data == T & last_obs_m > forecast_period_start_m | use_freshest_data == T & last_obs_m == forecast_period_start_m & last_obs_d >=  forecast_period_start_d){
    p_2_start_m<-month(max(series$date)+1)
    p_2_start_d<-mday(max(series$date)+1)
  }else{
    p_2_start_m<-forecast_period_start_m
    p_2_start_d<-forecast_period_start_d
  }
    
  series<-series%>%
    mutate(period = ifelse(month(date) < p_2_start_m | month(date) == p_2_start_m & mday(date) < p_2_start_d, 1, 2))
    
  
  if(last_obs_m < p_2_start_m | last_obs_m == p_2_start_m & last_obs_d < p_2_start_d){
    series<-series%>%
      filter(month(date) < month(last_obs_m)  | month(date) > month(p_2_start_m) | month(date) == month(last_obs_m) & mday(date) <= last_obs_d | month(date) == p_2_start_m & mday(date) >= p_2_start_d)
  }
  
  obs_period_2<-series%>%
    filter(period == 1)%>%
    mutate(obs_period_2 = ifelse(month(date) > forecast_period_start_m | month(date) == forecast_period_start_m & mday(date) >= forecast_period_start_d, 1, 0))%>%
    filter(obs_period_2 == 1)%>%
    group_by(year)%>%
    summarize(obs_abundance=sum(abundance),.groups="keep")
  
  series<-series%>%
    dplyr::group_by_at(c("year","species","period",all_of(covariates)))%>%
    summarize(abundance=sum(abundance),.groups="keep")
  
  # if(length(p1_covariates_only)>0){
  #   series[series$period==2,colnames(series)%in%p1_covariates_only]<-series[series$period==1,colnames(series)%in%p1_covariates_only]*-1
  # }
  if(length(p1_covariates_only)>0){
    series[series$period==2,colnames(series)%in%p1_covariates_only]<-series[series$period==2,colnames(series)%in%p1_covariates_only] * -1
  }
  
  
  out<-list(series = series, p_2_start_m = p_2_start_m, p_2_start_d = p_2_start_d, obs_period_2 = obs_period_2)
  
  return(out)
}