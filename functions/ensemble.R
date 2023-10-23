ensemble<-function(forecasts,series,TY_ensemble,k,slide){

  # forecasts<-forecasts %>% left_join(series %>% dplyr::select(year,abundance))

  yrrange<-forecasts%>%
    summarise(minyr=min(year),maxyr=max(year))%>%
    unlist()
  
  maxdata_year<-forecasts%>%
    filter(!is.na(abundance))%>%
    summarise(max(year))%>%
    unlist()%>%
    pluck()
  
  
  ensembles<-NULL
  for(i in (yrrange[2]-TY_ensemble):maxdata_year){
    years<-seq(to=i,length.out=slide)#c(yrrange[1]:i)
    
    tdat<-forecasts%>%
      dplyr::filter(
        model%in% c(forecasts %>% filter(year==i+1,rank<=num_models) %>% pull(model))
      ) %>% 
      filter(year %in% years)%>%
      left_join(series,by="year")%>%
      dplyr::select(year,model,predicted_abundance,abundance=abundance.x)%>%
      mutate(error=abundance-predicted_abundance)%>%
      filter(!is.na(error))%>%
      group_by(model)%>%
      summarise(RMSE = sqrt(mean(error^2)),
                MAPE = mean(abs(error/abundance))*100,
                MSA = 100*(exp(mean(abs(log(abundance/predicted_abundance))))-1)
      )%>%
      arrange(MSA)%>%
      mutate(MSA_weight=(1/MSA)^k/sum((1/MSA)^k), 
             RMSE_weight =(1/RMSE)^k/sum((1/RMSE)^k),
             MAPE_weight =(1/MAPE)^k/sum((1/MAPE)^k)
      )
    
    modelcnt<-num_models
    
    stackdat<-forecasts%>%
      dplyr::filter(
        model%in% c(forecasts %>% filter(year==i+1,rank<=num_models) %>% pull(model))
      ) %>% 
      filter(year %in% years)%>%
      pivot_wider(names_from = model, values_from = predicted_abundance,id_cols = year)%>%
      left_join(series%>%dplyr::select(year,abundance))
    
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
                                        as.matrix()
    )
    stacking_weights<-data.frame("Stacking_weight" = as.vector(round(unlist(stack_weights[[1]]),4)))
    stacking_weights$model<-colnames(stackdat)[!colnames(stackdat)%in%c("year","abundance")]
    tdat<-tdat%>%
      left_join(stacking_weights)
    
    tdat2<-forecasts%>%
      dplyr::filter(
        model%in% c(forecasts %>% filter(year==i+1,rank<=num_models) %>% pull(model))
      ) %>% 
      filter(year == max(years)+1)%>%
      pivot_longer(names_to = "Parameter",
                   cols=c("predicted_abundance","Lo 95","Hi 95", "Lo 50", "Hi 50"),
                   values_to = "value")%>%
      left_join(tdat %>% dplyr::select(model,MSA_weight:Stacking_weight))%>%
      mutate(MSA_weighted = value * MSA_weight,
             RMSE_weighted = value * RMSE_weight,
             MAPE_weighted = value * MAPE_weight,
             Stack_weighted = value * Stacking_weight,
      )%>%
      group_by(year,Parameter)%>%
      summarise(MSA_weighted = sum(MSA_weighted),
                RMSE_weighted = sum(RMSE_weighted),
                MAPE_weighted = sum(MAPE_weighted),
                Stack_weighted = sum(Stack_weighted),
      )%>%
      pivot_longer(names_to = "model",
                   cols=c("MSA_weighted","RMSE_weighted","MAPE_weighted","Stack_weighted"),
                   values_to = "value")%>%
      pivot_wider(id_cols = c("year","model"),names_from=Parameter,values_from=value)
    
    ensembles<-bind_rows(ensembles,tdat2)
  }
  
  
  
  forecast_skill<-evaluate_forecasts2(forecasts = bind_rows(forecasts, ensembles%>%
                                                              left_join(series))%>%
                                        filter(year>(yrrange[2]-TY_ensemble))
                                      ,observations = series)
  
  forecasts2<- bind_rows(forecasts,ensembles %>% left_join(series))%>%
    filter(year>(yrrange[2]-TY_ensemble))%>%
    mutate(error=predicted_abundance-abundance,
           pct_error=scales::percent(error/abundance)
           )%>%
    left_join(tdat%>%dplyr::select(model,Stacking_weight))
    
  
  results<-list(
    final_model_weights = tdat,
    forecast_skill = forecast_skill,
    ensembles = ensembles,
    forecasts=forecasts2
  )
  return(results)
}
