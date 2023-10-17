all_subsets<-function(series,   # data with annual values of response (abundance) and predictor covariates
                      covariates, # vector of covariates to include in models
                      min, # minimum number of covariates within individual model
                      max, # maximum number of covariates within individual model
                      type, # specifies whether an inseason or preseason forecast
                      fit=TRUE # flag for whether to fit models
                      ){ 
  #is
  freq=ifelse(type=="inseason",2,1)
  seasonal=ifelse(type=="inseason",T,F)
  
  #processing data
  series<-series%>%
    ungroup()%>%
    dplyr::select(year,species,period,abundance,all_of(covariates))%>%
    filter(
      across(
        .cols = all_of(covariates),
        .fns = ~ !is.na(.x)
      )
    )
  
  #empty object to hold outputs
  vars<-list()
  AICc=c()
  formula=c()
  model_num = c()
  tmin<-ifelse(min==0,1,min)
  
  for(i in tmin:max){#loop over number of covariates pre model
    temp<-combn(covariates,i, simplify = FALSE) # create all unique combinations of length i
    for(j in 1:length(temp)){
      vars[[length(vars) + 1]]<-temp[[j]]
    }
  }
  
  
  total = ifelse(min==0,length(vars)+1,length(vars))
  print(paste0("There are ",total," models to fit! Fitting model number:"))
  
  if(fit){
  for(i in 1:length(vars)){
    print(paste0(i," out of ",total))
    #arma<-data.frame(NA,nrow=length(vars),ncol=7)
    #colnames(arma)<-c("p","d","q","P","D","Q")
    xreg<-series%>%
      ungroup%>%
      dplyr::select(all_of(vars[[i]]))%>%
      as.matrix()
    
    m1<-series%>%
      ungroup()%>%
      dplyr::select(abundance)%>%
      unlist()%>%
      ts(frequency = freq)%>%
      auto.arima(lambda=0,seasonal = seasonal, xreg = xreg)
    AICc[i]<-m1$aicc
    #arma[i,]<-arimaorder(m1)
    formula[i]<-paste0("abundance ~ ",paste(all_of(vars[[i]]),collapse = " + "))
    model_num[i]<-i
  }
  if(min==0){
    i = total
    print(paste0(i," out of ", total))
    m1<-series%>%
      ungroup()%>%
      dplyr::select(abundance)%>%
      unlist()%>%
      ts(frequency = freq)%>%
      auto.arima(lambda=0,seasonal = seasonal, xreg = NULL)
    AICc[i]<-m1$aicc
    #arma[i,]<-arimaorder(m1)
    formula[i]<-"abundance ~ 1"
    model_num[i]<-i
  }
  table<-as_tibble(data.frame(model_num,AICc,formula))%>%
    arrange(AICc)}else{
      table<-NULL
    }
  results<-list(vars,table)
  return(results)
}

