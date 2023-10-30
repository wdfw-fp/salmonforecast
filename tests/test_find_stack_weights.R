# Source the script containing the all_subsets function
source("functions/all_subsets.r")
source("functions/prepare_data.r")
#source("functions/find_stack_weights.r")

test_that("Test find_stack_weights function", {
  install_or_load_pack <- function(packs) {
    for (pack in packs) {
      # Check if the package is already installed
      if (!requireNamespace(pack, quietly = TRUE)) {
        # If not installed, install it
        install.packages(pack, dependencies = TRUE)
      }

      # Load the package
      library(pack, character.only = TRUE)
    }
  }

  # Usage example:
  packages_list<-c("tidyverse"
                   ,"forecast"
                   ,"mgcv"
                   ,"ggplot2"
                   ,"MASS"
                   ,"RColorBrewer"
                   ,"kableExtra"
                   ,"lubridate"
                   ,"modelr"
                   ,"kableExtra"
                   ,"reshape2"
                   ,"ggfortify"
                   ,"clock"
                   ,"smooth"
                   ,"scales"
                   ,"here"
                   ,"MuMIn"
                   ,"gtools"
  )
  install_or_load_pack(packs = packages_list)
  #=========
  # Raw Data
  #=========
  yr_start<-1992
  yr_end<-year(Sys.Date())
  dam="Baker"
  species="Sock"
  #===========================
  # Summarization for analysis
  #===========================
  date_start_analysis<-ymd("1992/6/1") #this will be the first date of data used in the analysis (and this month/day first in years thereafter)
  date_end_analysis<-ymd("2022/12/31") #this will be the last date of data used in the analysis (and this month/day last in years preceding)
  forecast_period_start_m<-6 #this will be the month associated with the first month/day of the seasonal estimate period each year...can be after first data used to estimate it or the same
  forecast_period_start_d<-1 #this will be the month day associated with the first month/day of the seasonal estimate period each year...can be after first data used to estimate it or the same
  use_freshest_data = T #use all data up to "today" or only up to beginning of forecast period
  last_data<-as.Date("2022-07-14")
  #==================
  #forecasting params
  #==================
  leave_yrs<- 11#11
  covariates<-c("lag2_NPGO","lag1_log_SAR2","lag2_log_smolts","var_flow", "lag1_PDO","lag1_NPGO","lag2_PDO","pink_ind","lag1_log_SAR1","zl_flow")
  p1_covariates_only=c("var_flow","zl_flow")
  plot_results = F
  first_forecast_period = 2
  write_model_summaries = TRUE
  find_best = T

  # From 2013/2014 SRSC study:
  area8TravelTime <- 15
  area78CTravelTime <- 12
  area78D2TravelTime <- 10
  area78D3TravelTime <- 10
  area78D478OTravelTime <- 5
  area78D5TravelTime <- 5


  dat<-read_csv(here("data","tbl_totalSockeyeCountByArea.csv"))%>%
    #   mutate(TreatyA8 = lag(TreatyA8,area8TravelTime),TreatyA78C = lag(TreatyA78C,area78CTravelTime),TreatyA78D2 = lag(TreatyA78D2,area78D2TravelTime),TreatyA78D3 = lag(TreatyA78D3,area78D3TravelTime),TreatyA78D478O =
    # lag(TreatyA78D478O,area78D478OTravelTime),TreatyA78D5 = lag(TreatyA78D5,area78D5TravelTime),SportA78D2 = lag(SportA78D2
    # ,area78D2TravelTime),SportA78D478O = lag(SportA78D478O,area78D478OTravelTime)) %>%
    pivot_longer(-c("ID","CountDate","Comment"),names_to = "location",values_to = "count")%>%
    mutate(date=mdy(CountDate),year=year(date))%>%
    mutate()%>%
    group_by(date,year)%>%
    summarise(abundance=sum(count,na.rm = TRUE), .groups = "keep")%>%
    mutate(species=ifelse(abundance >= 0,"Sockeye","Sockeye"))%>%
    filter(!is.na(date))


  #=========================================================
  #get PDO data
  #=========================================================
  PDO<-read_table("https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv",skip=1,col_names=F,comment="#")%>%
    dplyr::rename(Date=X1,PDO=X2)%>%
    filter(!PDO < -99)%>%
    mutate(Date=as.Date(Date),Month=month(Date),Year=year(Date))%>%
    group_by(Year)%>%
    add_tally()%>%
    #filter(!Month>6)%>% #use only spring (Jan-June) NPGO
    #filter(!n < 12)%>% #use only complete years
    group_by(Year)%>%
    dplyr::summarise(PDO=mean(PDO))%>%
    mutate(lag2_PDO = lag(PDO,2), lag1_PDO = lag(PDO,1))%>%
    dplyr::select(year=Year,lag2_PDO, lag1_PDO)
  #=========================================================
  #get NPGO data
  #=========================================================
  NPGO<-read_table("http://www.o3d.org/npgo/npgo.php",skip=29,col_names=F,comment="#")%>%
    filter(!is.na(X2))%>%
    dplyr::rename(Year=X1,Month=X2,NPGO=X3)%>%
    mutate(Year=as.numeric(Year))%>%
    group_by(Year)%>%
    add_tally()%>%
    #filter(!Month>6)%>% #use only spring (Jan-June) NPGO
    filter(!n < 12)%>% #use only complete years
    group_by(Year)%>%
    dplyr::summarise(NPGO=mean(NPGO))%>%
    mutate(Year=Year+1, lag1_NPGO = NPGO,lag2_NPGO = lag(NPGO))%>%
    dplyr::select(year=Year,lag1_NPGO,lag2_NPGO)
  #=========================================================
  #get Salmon Ocean Abundance Data
  #=========================================================
  OceanSalmon<-read_csv("https://raw.githubusercontent.com/tbuehrens/Salmon_Forecast_Example/main/data/pink_covariates_1952-2020.csv")%>%
    dplyr::rename(Year=year)%>%
    mutate(log_tot_pk_num = log(tot_pk_num))%>%
    dplyr::select(year=Year,log_tot_pk_num)
  #=========================================================
  #get SAR survival/age data
  #=========================================================
  SAR<-read_csv(here("data","baker_sockeye_SAR.csv"))%>%
    filter(Year<2021)


  SAR<-SAR%>%bind_cols(SAR1=data.frame(SAR1=gam(cbind(round(OA1_Recruits),smolts-round(OA1_Recruits))~s(Year,k=(dim(SAR)[1]),m=1,bs="ps"),family=quasibinomial,data=SAR)$fitted))%>%
    bind_cols(SAR2=data.frame(SAR2=c(NA,gam(cbind(round(OA2_Recruits),smolts-round(OA2_Recruits))~s(Year,k=(dim(SAR)[1]-1),m=1,bs="ps"),family=quasibinomial,data=SAR)$fitted)))%>%
    mutate(year=Year+2,lag1_log_SAR1 = lag(log(SAR1),1),lag1_log_SAR2=log(SAR2))%>%
    dplyr::select(year=year,lag1_log_SAR1,lag1_log_SAR2)

  #=========================================================
  #get smolts
  #=========================================================
  Smolts<-read_csv(here("data","baker_sockeye_SAR.csv"))%>%
    dplyr::mutate(year=Year+2,lag2_log_smolts=log(smolts))%>%
    dplyr::select(year,lag2_log_smolts)



  ## try with baker river flows
  flow_site<-12193400
  flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv?&format=rdb&site_no=",flow_site,
                     "&period=&begin_date=",yr_start,"-01-01",
                     "&end_date=",yr_end,"-12-31")
  flow<-readr::read_delim(flow_url,comment = '#')%>%
    filter(agency_cd=="USGS")%>%
    dplyr::rename(date=datetime,CFS=`149403_00060_00003`)%>%
    dplyr::select(date,CFS)%>%
    mutate(date=ymd(date),CFS = as.numeric(CFS),flow_diff = log(lag(CFS,1)) - log(CFS))


  flow<-flow%>%
    mutate(year=year(date),month=month(date),yday=yday(date))%>%
    filter(yday <= yday(last_data) & yday >= yday(last_data-13))%>%
    group_by(year)%>%
    dplyr::summarise(zl_flow=mean(log(CFS),na.rm=T),var_flow=sd(flow_diff,na.rm=T),.groups = "keep")%>%
    ungroup()%>%
    mutate(zl_flow=as.vector(scale(zl_flow)),var_flow = as.vector(scale(var_flow)))

  #================================================================
  dat<-dat%>%
    left_join(PDO)%>%
    left_join(NPGO)%>%
    left_join(OceanSalmon)%>%
    left_join(SAR)%>%
    left_join(Smolts)%>%
    left_join(flow)%>%
    mutate(pink_ind = ifelse(year< 1999 | year%%2==0,0,1))

  timing<-dat%>%
    mutate(yday=yday(date))%>%
    group_by(year)%>%
    mutate(cumpct=cumsum(abundance)/sum(abundance),diff50=abs(0.5-cumpct),date50=ifelse(diff50==min(diff50),1,0))%>%
    filter(date50==1 & year < yr_end)%>%
    summarise(yday=mean(yday))

  dat<-dat%>%
    filter(date <= last_data)


  summarized_data<-prepare_data(series = dat,
                                date_start_analysis = date_start_analysis,
                                date_end_analysis = date_end_analysis,
                                forecast_period_start_m = forecast_period_start_m, #inclusive
                                forecast_period_start_d = forecast_period_start_d, #inclusive
                                use_freshest_data = use_freshest_data,
                                covariates = covariates,
                                p1_covariates_only = p1_covariates_only
  )

  if(find_best ==T){
    best_covariates<-all_subsets(series=summarized_data$series,covariates=covariates,min=0,max=5,type = "preseason",
                                 fit = TRUE)
    saveRDS(best_covariates,"tests/outputs/best_covariates_Baker_find_stack_weights.rds")
  }


  best_covariates<-readRDS("tests/outputs/best_covariates_Baker_find_stack_weights.rds")

  series=summarized_data$series
  leave_yrs=leave_yrs
  covariates= best_covariates[[1]][best_covariates[[2]]$model_num[1:10]]
  first_forecast_period = first_forecast_period
  plot_results = plot_results
  write_model_summaries = write_model_summaries
  forecast_period_start_m =  forecast_period_start_m #inclusive
  forecast_period_start_d =  forecast_period_start_d #inclusive
  obs_period_2 = summarized_data$obs_period_2
  p1_covariates_only = p1_covariates_only
  stack_metric = "MSA"

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


  expect_true(!is.null(stack_weights))

  # Add more expectations as needed to check specific aspects of the results
  # Save the results to a CSV file
  write.csv(stack_weights, file = "tests/outputs/find_stack_weights.csv", row.names = FALSE)

  # You can add more test assertions here if needed

})
