arima_forecast<-function(tdat,xreg,xreg_pred,last_train_yr,first_forecast_period){
  result<-tryCatch(
    {
      if(ncol(xreg)>0){
        m1<-tdat%>%
          filter(train_test==0)%>%
          ungroup()%>%
          dplyr::select(abundance)%>%
          unlist()%>%
          ts(frequency = 2)%>%
          auto.arima(lambda=0,seasonal = T, xreg = xreg)

        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2,
                                             xreg = xreg_pred
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2, level = c(50, 95),
                               xreg = xreg_pred
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(year = last_train_yr+1, period = ifelse(first_forecast_period == 2,2,c(1:2)))
      }else{
        m1<-tdat%>%
          filter(train_test==0)%>%
          ungroup()%>%
          dplyr::select(abundance)%>%
          unlist()%>%
          ts(frequency = 2)%>%
          auto.arima(lambda=0,seasonal = T, xreg = NULL)

        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=1,
                                             xreg = NULL
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=1, level = c(50, 95),
                               xreg = NULL
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(year = last_train_yr+1, period = 1)
      }
      return(list(pred = pred, CI = CI, arma=paste(m1$arma,collapse = ""),
                  aicc=m1$aicc))
      if(write_model_summaries ==T){
        sink("summary.txt",append=T)
        print(summary(m1))
        sink()
      }
    },error = function(e){
      # If an error occurs, print a message and return NAs for pred and CI
      message("An error occurred while fitting or forecasting the model.")
      message("Here is the original error message:")
      message(e)
      return(list(pred = rep(NA,tdat%>%filter(train_test==0)%>%nrow()+1),
                  CI = rep(NA,4)%>%
                    as_tibble()%>%
                    bind_cols(names=c("Lo 50","Hi 50","Lo 95","Hi 95"))%>%
                    mutate(value=as.numeric(value))%>%
                    pivot_wider(names_from = names,values_from = value)%>%
                    mutate(year = last_train_yr+1, period = 1)
                  ,
                  arma=paste(m1$arma,collapse = ""),
                  aicc=m1$aicc)
      )
    }
  )
  return(result)
}
