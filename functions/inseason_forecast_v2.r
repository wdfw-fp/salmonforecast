#function to evaluate performance of SARIMA model (produces season total forecasts only)
inseason_forecast_v2<-function(series,
                            leave_yrs,
                            covariates,
                            first_forecast_period,
                            plot_results,
                            write_model_summaries,
                            forecast_period_start_m, #inclusive
                            forecast_period_start_d, #inclusive
                            obs_period_2,
                            p1_covariates_only,
                            stack_metric
                            ){
  if(write_model_summaries ==T){
    write.table(NULL,"summary.txt")
  }
  
  series<-series%>%
    ungroup()%>%
    dplyr::select(year,species,period,abundance,all_of(unique(unlist(covariates))))%>%
    filter(
      across(
        .cols = all_of(unique(unlist(covariates))),
        .fns = ~ !is.na(.x)
      )
    )
  
  for(c in 1:length(covariates)){
    for(i in 1:leave_yrs){
      last_train_yr = max(series$year) - (leave_yrs-i+1)
      tdat<-series%>%
        filter(year <= (last_train_yr + 1))%>%
        mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0),
        )
      
      exists1<-tdat%>%filter(year==(last_train_yr+1) & period == 1)%>%ungroup%>%dplyr::select(abundance)%>%nrow()
      if(exists1==0){
        adddat<-tdat%>%tail(1)
        adddat$period<-1
        adddat$abundance<-NA
        adddat$train_test<-1
        tdat<-tdat%>%
          bind_rows(adddat)
      }
      exists2<-tdat%>%filter(year==(last_train_yr+1) & period == 2)%>%ungroup%>%dplyr::select(abundance)%>%nrow()
      if(exists2==0){
        adddat<-tdat%>%tail(1)
        adddat$period<-2
        adddat$abundance<-NA
        adddat$train_test<-1
        tdat<-tdat%>%
          bind_rows(adddat)
      }
      
      if(length(p1_covariates_only)>0){
        tdat[tdat$period==2,colnames(tdat)%in%p1_covariates_only]<-tdat[tdat$period==1,colnames(tdat)%in%p1_covariates_only]*-1
      }
      
      xreg<-tdat%>%
        filter(train_test==0)%>%
        ungroup%>%
        dplyr::select(all_of(covariates[[c]]))%>%
        as.matrix()
      
      xreg_pred<-tdat%>%
        filter(train_test==1)%>%
        ungroup%>%
        dplyr::select(all_of(covariates[[c]]))%>%
        as.matrix()
  
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
        
        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2,
                                             xreg = NULL
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=(1/first_forecast_period)*2, level = c(50, 95),
                               xreg = NULL
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(year = last_train_yr+1, period = ifelse(first_forecast_period == 2,2,c(1:2)))
      }
      
      if(write_model_summaries ==T){
        sink("summary.txt",append=T)
        print(summary(m1))
        sink()
      }
      
      tdat<-tdat%>%
        bind_cols(pred=pred)%>%
        left_join(CI, by = c("year","period"))%>%
        dplyr::rename(predicted_abundance = pred)%>%
        filter(train_test==1)
      
      if(i==1){forecasts = tdat
      }else{forecasts = forecasts %>% bind_rows(tdat)}
    }
    
    if(nrow(obs_period_2)>0){
      forecasts<-forecasts%>%
        left_join(obs_period_2)%>%
        mutate(
          abundance = abundance + obs_abundance,
          predicted_abundance = predicted_abundance + obs_abundance,
          `Lo 50` = `Lo 50` + obs_abundance,
          `Lo 95` = `Lo 95` + obs_abundance,
          `Hi 50` = `Hi 50` + obs_abundance,
          `Hi 95` = `Hi 95` + obs_abundance,
          )
    }else{
      forecasts<-forecasts%>%
        mutate(obs_abundance = 0)
    }
    forecasts<-forecasts%>%
      mutate(error = predicted_abundance-abundance,
             pct_error=scales::percent(error/abundance),
             model = as.character(c)
      )
    if(c==1){
      tdat2 <- forecasts
    }else{
      tdat2 <- tdat2%>%
        bind_rows(forecasts)
    }
  }
  forecasts<-tdat2
  
  modelcnt<-length(unique(forecasts$model))
  
  stackdat<-forecasts%>%
    ungroup()%>%
    dplyr::select(year,abundance,predicted_abundance,model)%>%
    filter(!is.na(abundance))%>%
    pivot_wider(names_from = model,values_from = predicted_abundance)
  
  stack_weights<-find_stack_weights(tau=1,
                     n=10000,
                     metric=stack_metric,
                     initial_weights=rep(1/modelcnt,modelcnt),
                     preds=stackdat%>%
                       filter(!is.na(abundance))%>%
                       dplyr::select(!abundance & !year)%>%
                       as.matrix(),
                     obs=stackdat%>%
                       filter(!is.na(abundance))%>%
                       dplyr::select(abundance & !year)%>%
                       as.matrix())
  
  stacking_weights<-data.frame("Stacking_weight" = as.vector(round(unlist(stack_weights[[1]]),4)))
  stacking_weights$model<-colnames(stackdat)[!colnames(stackdat)%in%c("year","abundance")]
  tdat2<-forecasts%>%
    left_join(stacking_weights)%>%
    pivot_longer(names_to = "Parameter",
                 cols=c("predicted_abundance","Lo 95","Lo 50","Hi 50", "Hi 95"),
                 values_to = "value")%>%
    left_join(tdat)%>%
    mutate(Stack_weighted = value * Stacking_weight,
    )%>%
    group_by(year,Parameter)%>%
    summarise(Stack_weighted = sum(Stack_weighted),
    )%>%
    pivot_longer(names_to = "model",
                 cols=c("Stack_weighted"),
                 values_to = "value")%>%
    pivot_wider(id_cols = c("year","model"),names_from=Parameter,values_from=value)%>%
    left_join(forecasts%>%dplyr::select(model,year,abundance,obs_abundance,species,all_of(unique(unlist(covariates))))%>%filter(model==as.character(c))%>%dplyr::select(!model))%>%
    mutate(error = predicted_abundance-abundance,
           pct_error=scales::percent(error/abundance),
           model = "ensemble"
    )
  
  forecasts<-bind_rows(forecasts,tdat2)%>%
    left_join(stacking_weights)
  
  return(forecasts)
}

