# Source the script containing the all_subsets function
source("R/all_subsets.r")
source("R/evaluate_forecasts2.r")
source("R/one_step_ahead.r")
source("R/arima_forecast.r")
test_that("Test rolling performance function", {
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

  packages_list<-c("tidyverse"
                   ,"forecast"
                   ,"mgcv"
                   ,"ggplot2"
                   ,"MASS"
                   ,"RColorBrewer"
                   ,"kableExtra"
                   ,"modelr"
                   ,"kableExtra"
                   ,"reshape2"
                   ,"ggfortify"
                   ,"clock"
                   ,"smooth"
                   ,"scales"
                   ,"gtools"
                   ,"here"
                   ,"MuMIn"
                   ,"janitor"
                   ,"rvest"
                   ,"lubridate"
                   ,"rnoaa"
                   ,"ncdf4"
                   ,"magrittr"
                   ,"ggcorrplot"
                   ,"parallel"
                   ,"foreach"
                   ,"doParallel"
  )
  install_or_load_pack(pack = packages_list)

  yr_start<-1967
  yr_end<-year(Sys.Date())
  #dam="BON"
  species="Coho"
  #===========================
  # Summarization for analysis
  #===========================
  date_start_analysis<-ymd("1967/1/1") #this will be the first date of data used in the analysis (and this month/day first in years thereafter)
  date_end_analysis<-ymd("2023/12/31") #this will be the last date of data used in the analysis (and this month/day last in years preceding)
  forecast_period_start_m<-1 #this will be the month associated with the first month/day of the seasonal estimate period each year...can be after first data used to estimate it or the same
  forecast_period_start_d<-1 #this will be the month day associated with the first month/day of the seasonal estimate period each year...can be after first data used to estimate it or the same
  last_data<-Sys.Date()
  #==================
  #forecasting params
  #==================
  leave_yrs<- 31
  TY_ensemble<-16
  k<-1
  covariates<-c(#new
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
    ,"UWI.SON"

    #Very correlated with variables already included
    #,"lag1_fall_Nino3.4"
    #,"SST.J"

    #Original OPIT indicators very correlated with variables already used(but untransformed)
    #"lag1_JackOPI"
    #,"lag1_SmAdj"

    #NOAA Ocean Indicators
    #,"lag1_sp_phys_trans"
    #,"lag1_PC1"
  )
  plot_results = F
  first_forecast_period = 1
  write_model_summaries = TRUE
  find_best=T
  #==============
  #Ensemble Params
  #===============
  min_vars<-0
  max_vars<-1
  forecast_type<-"preseason"
  stack_metric<-"MAPE"
  num_models<-10
  rolling_year_window<-15

  inputs<-list(
    yr_start = yr_start,
    yr_end = yr_end,
    species = species,
    date_start_analysis = date_start_analysis,
    date_end_analysis = date_end_analysis,
    forecast_period_start_m = forecast_period_start_m,
    forecast_period_start_d = forecast_period_start_d,
    last_data = last_data,
    leave_yrs = leave_yrs,
    TY_ensemble = TY_ensemble,
    k = k,
    covariates = covariates,
    plot_results = plot_results,
    first_forecast_period = first_forecast_period,
    write_model_summaries = write_model_summaries,
    find_best = find_best,
    min_vars = min_vars,
    max_vars = max_vars,
    forecast_type = forecast_type,
    stack_metric = stack_metric,
    num_models = num_models,
    rolling_year_window
  )



  make_dat<-function(file_path=NULL){


    if(!is.null(file_path)){
      return(read_csv(file_path))

    }else{
      OPIHData <- read.table("https://raw.githubusercontent.com/ErikSuring/OPIH_Evaluation/main/PUB2023.txt", header = TRUE, sep = "\t", strip.white = TRUE, comment.char = "#")

      nYears <- length(OPIHData[,1])
      earliestYear <- OPIHData[1,1]
      latestYear <- OPIHData[nYears,1]

      OPIHData$AdltAll <- c(OPIHData$AdltSRS[1:17],OPIHData$AdltMSM[18:nYears])   # row 18  <-  1986. Use SRS data prior, MSM data post
      OPIHData$AdltAllno83 <- OPIHData$AdltAll
      OPIHData$AdltAllno83[15] <-  NA   #  1983 is removed from model input data
      OPIHData[,"AdltAll"] <- OPIHData$AdltAll
      OPIHData[,"AdltAllno83"] <- OPIHData$AdltAllno83

      # Lag Jack and Smolt data to return year
      OPIHData[,"lagJackCR"] <- c(NA, OPIHData$JackCR[1:nYears-1])
      OPIHData[,"lagJackOC"] <- c(NA, OPIHData$JackOC[1:nYears-1])
      OPIHData[,"lagJackOPI"] <- OPIHData$lagJackCR + OPIHData$lagJackOC

      OPIHData[,"lagSmD"] <- c(NA, OPIHData$SmD[1:nYears-1])
      OPIHData[,"lagSmCR"] <- c(NA, OPIHData$SmCR[1:nYears-1])
      OPIHData[,"lagSmAdj"] <- OPIHData$lagJackCR*(OPIHData$lagSmD/OPIHData$lagSmCR)   #  Calculate Smolt Adjustment

      # Output OPIHData as a .csv file
      #write.table(OPIHData, file = "OPIHData.csv", sep = ", ", row.names=FALSE)

      OPIHData%<>%
        dplyr::rename(year=Year,
                      abundance = AdltAllno83
        )%>%
        as_tibble()%>%
        arrange(year)

      Yrlist<-data.frame(year=c(min(OPIHData$year):(max(OPIHData$year)+1)))

      OPIHData%<>%
        right_join(Yrlist)%>%
        arrange(year)
      #=========================================================
      #get PDO data
      #=========================================================
      PDO<-read_table("https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv",skip=1,col_names=F,comment="#")%>%
        dplyr::rename(Date=X1,PDO=X2)%>%
        filter(!PDO < -99)%>%
        mutate(Date=as.Date(Date),Month=month(Date),Year=as.integer(year(Date)))%>%
        group_by(Year)%>%
        add_tally()%>%
        #filter(!Month>6)%>% #use only spring (Jan-June) NPGO
        #filter(!n < 12)%>% #use only complete years
        group_by(Year)%>%
        dplyr::rename(year=Year)%>%
        dplyr::summarise(PDO=mean(PDO))%>%
        mutate(lag1_PDO = lag(PDO,1))%>%
        right_join(Yrlist)%>%
        dplyr::select(year,lag1_PDO)%>%
        filter(!is.na(lag1_PDO))
      #=========================================================
      #get NPGO data
      #=========================================================
      NPGO<-read_table("http://www.o3d.org/npgo/npgo.php",skip=29,col_names=F,comment="#")%>%
        filter(!is.na(X2))%>%
        dplyr::rename(Year=X1,Month=X2,NPGO=X3)%>%
        mutate(Year=as.integer(as.numeric(Year)))%>%
        group_by(Year)%>%
        add_tally()%>%
        #filter(!Month>6)%>% #use only spring (Jan-June) NPGO
        #filter(!n < 12)%>% #use only complete years
        group_by(Year)%>%
        dplyr::summarise(NPGO=mean(NPGO))%>%
        dplyr::rename(year=Year)%>%
        mutate(lag1_NPGO = lag(NPGO))%>%
        right_join(Yrlist)%>%
        arrange(year)%>%
        dplyr::select(year,lag1_NPGO)%>%
        filter(!is.na(lag1_NPGO))
      #=========================================================
      #get NOAA indicator data, wrangle into usable format, plot
      #=========================================================
      indicators<-read_csv("https://www.fisheries.noaa.gov/s3//2022-12/OEI-spotlight-cvs-2022-NWFSC.csv",skip=1)%>%
        filter(!is.na(`Ecosystem Indicators`))%>%
        pivot_longer(names_to = "Year",
                     cols=c(starts_with("1"),starts_with("2")),
                     values_to = "value")%>%
        pivot_wider(names_from=`Ecosystem Indicators`,values_from=value)%>%
        mutate(year=as.integer(Year))%>%
        dplyr::select(-Year)%>%
        right_join(Yrlist)%>%
        arrange(year)%>%
        mutate(lag1_PC1 = lag(scale(`Principal Component scores (PC1)`)[,1]))%>%
        filter(!is.na(lag1_PC1))
      #=========================================
      # Get ENSO index
      #=========================================
      enso<-read_table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino34.long.anom.data",skip=1,col_names = F)%>%
        as_tibble()%>%
        setNames(c("year",1:12))%>%
        filter(year%in%as.character(yr_start:yr_end))%>%
        mutate(across(everything(),~as.numeric(.)))%>%
        pivot_longer(names_to = "month",values_to = "Nino3.4",cols=c(!year))%>%
        filter(Nino3.4>-10 & Nino3.4 <10)%>%
        filter(month%in%c(10:12))%>%
        group_by(year)%>%
        summarise(fall_Nino3.4=mean(Nino3.4))%>%
        right_join(Yrlist)%>%
        mutate(lag1_fall_Nino3.4=lag(fall_Nino3.4))%>%
        dplyr::select(-fall_Nino3.4)

      #===========================================================================================================
      #Get ERSST data: get_ersst_v5_data takes A LONG TIME (1 hr) vs. get_ersst_v5_data_V2 which is much quicker!
      #==========================================================================================================
      sstdat<-get_ersst_v5_data_V2(years=c(min(Yrlist$year):max(Yrlist$year)),
                                   data.dir="https://stateofwa.sharepoint.com/:u:/r/sites/DFW-TeamWDFWOPIModelReview/Shared%20Documents/General/sst.mnmean.nc?csf=1&web=1&e=mhx1bc",
                                   #"C:\\Users\\sorelmhs\\Washington State Executive Branch Agencies\\DFW-Team WDFW OPI Model Review - General" ,
                                   ncfilename="sst.mnmean.nc",
                                   latrange=c(44,50),
                                   lonrange=c(-125,-120)
      )

      ssta<-sstdat%>%
        dplyr::select(year,month,resid)%>%
        mutate(month=as.numeric(month))%>%
        mutate(year=ifelse(month>9,year+1,year))%>%
        group_by(year)%>%
        summarise(WSST_A = mean(resid[month>9|month<2]),
                  SSST_A = mean(resid[month<=9|month>=4]),
        )

      #==========================================
      #Get OCN covariates
      #==========================================
      OCN<-read_csv("data/OCNR_Forecast_Summary.csv")%>%
        as_tibble()%>%
        dplyr::rename(year=YEAR)%>%
        dplyr::select(-c("ADULTS","SPAWNERS","Spawners"))
      #================================================================
      dat<-OPIHData%>%
        left_join(Yrlist)%>%
        left_join(PDO)%>%
        left_join(NPGO)%>%
        left_join(indicators)%>%
        left_join(enso)%>%
        left_join(ssta)%>%
        left_join(OCN)%>%
        dplyr::rename(lag1_JackOPI = lagJackOPI,
                      lag1_SmAdj = lagSmAdj
        )%>%
        mutate(species = "Coho",
               period = 1,
               lag1_log_JackOPI = log(lag1_JackOPI),
               lag1_log_SmAdj = log(lagJackCR) * (lagSmD/lagSmCR),
               lag1_sp_phys_trans = lag(`Physical Spring Trans.\nUI based (day of year)`)
        )%>%
        ungroup()%>%
        dplyr::select(year,species,period,abundance,all_of(unique(unlist(covariates))))%>%
        filter(
          across(
            .cols = all_of(unique(unlist(covariates))),
            .fns = ~ !is.na(.x)
          )
        )#%>%mutate(across(!c(year,species,period,abundance),boxcox_scale(.)))
    }
  }

  dat<-make_dat("data/dat.csv")

  dat%>%
    dplyr::select(-c(year,species,period,abundance))%>%
    cor()%>%
    ggcorrplot(hc.order = TRUE, type = "lower", outline.col = "white", p.mat = NULL, sig.level = 0.05)

  dat%>%
    dplyr::select(-c(year,species,period,abundance))%>%
    cor()%>%
    round(.,2)



  {
    OPIHData <- read.table("https://raw.githubusercontent.com/ErikSuring/OPIH_Evaluation/main/PUB2023.txt", header = TRUE, sep = "\t", strip.white = TRUE, comment.char = "#")

    nYears <- length(OPIHData[,1])
    earliestYear <- OPIHData[1,1]
    latestYear <- OPIHData[nYears,1]

    OPIHData$AdltAll <- c(OPIHData$AdltSRS[1:17],OPIHData$AdltMSM[18:nYears])   # row 18  <-  1986. Use SRS data prior, MSM data post
    OPIHData$AdltAllno83 <- OPIHData$AdltAll
    OPIHData$AdltAllno83[15] <-  NA   #  1983 is removed from model input data
    OPIHData[,"AdltAll"] <- OPIHData$AdltAll
    OPIHData[,"AdltAllno83"] <- OPIHData$AdltAllno83

    # Lag Jack and Smolt data to return year
    OPIHData[,"lagJackCR"] <- c(NA, OPIHData$JackCR[1:nYears-1])
    OPIHData[,"lagJackOC"] <- c(NA, OPIHData$JackOC[1:nYears-1])
    OPIHData[,"lagJackOPI"] <- OPIHData$lagJackCR + OPIHData$lagJackOC

    OPIHData[,"lagSmD"] <- c(NA, OPIHData$SmD[1:nYears-1])
    OPIHData[,"lagSmCR"] <- c(NA, OPIHData$SmCR[1:nYears-1])
    OPIHData[,"lagSmAdj"] <- OPIHData$lagJackCR*(OPIHData$lagSmD/OPIHData$lagSmCR)   #  Calculate Smolt Adjustment

    # Output OPIHData as a .csv file
    #write.table(OPIHData, file = "OPIHData.csv", sep = ", ", row.names=FALSE)

    OPIHData%<>%
      dplyr::rename(year=Year,
                    abundance = AdltAllno83
      )%>%
      as_tibble()%>%
      arrange(year)

    Yrlist<-data.frame(year=c(min(OPIHData$year):(max(OPIHData$year)+1)))

    OPIHData%<>%
      right_join(Yrlist)%>%
      arrange(year)
    }

  if(find_best ==T){
    best_covariates<-all_subsets(series=dat,covariates=covariates,min=min_vars,max=max_vars,type=forecast_type,fit=FALSE)
    saveRDS(best_covariates,"tests/outputs/best_covariates_OPI_preseason_rolling_perf.rds")
  }

  best_covariates<-readRDS("tests/outputs/best_covariates_OPI_preseason_rolling_perf.rds")



  model_list<-lapply(best_covariates[[1]],#[best_covariates[[2]]$model_num[1:num_models]],
                     function(x) paste(x,collapse = " + "))%>%
    unlist()%>%
    as_tibble()%>%
    add_rownames()%>%
    dplyr::rename(model=rowname,model_name=value)






  fit_one_step<-T
  if(fit_one_step){


    results<-one_step_ahead(series=dat,
                            leave_yrs=leave_yrs,
                            TY_ensemble=TY_ensemble,
                            # covariates= best_covariates[[1]][best_covariates[[2]]$model_num[1:num_models]],
                            best_covariates[[1]],
                            first_forecast_period = first_forecast_period,
                            plot_results = plot_results,
                            write_model_summaries = write_model_summaries,
                            forecast_period_start_m =  forecast_period_start_m, #inclusive
                            forecast_period_start_d =  forecast_period_start_d, #inclusive
                            stack_metric = stack_metric,
                            k=k
    )

    write.csv(results,"tests/outputs/forecasts_rolling_perf.csv")

    save(results,file="tests/outputs/forecasts_rolling_perf.rds")
  }else{
    results<-read_csv("tests/outputs/forecasts_rolling_perf.csv")
  }


  # rolling_perf<-function(one_aheads,series,roll_years=15,mod_include=5){
  #
  #   out<-one_aheads  %>%left_join(series %>% dplyr::select(year,abundance)) %>%
  #     mutate(error=abundance-predicted_abundance,
  #            APE=abs(error/abundance))%>%
  #     arrange(model,year) %>%
  #     group_by(model) %>%
  #     mutate(MAPE= lag( 100*zoo::rollmean(APE, k = roll_years, fill = NA, align = "right"))) %>%
  #     group_by(year) %>%
  #     mutate(rank=rank(MAPE),
  #            model=as.character(model))
  #
  #
  #   #top performing models in each window
  #   tops<-out%>%
  #     filter(between(year,2023-TY_ensemble+1,2023),
  #            rank<=mod_include) %>%
  #     left_join(model_list) %>%
  #     arrange(desc(year),rank) #%>% dplyr::select(-1)
  #
  #
  #
  #   # performance of best model
  #   perf<-tops%>%
  #     ungroup %>%
  #     filter(rank==1) %>%
  #     summarize(MAPE=mean(APE,na.rm=T)*100,
  #               RMSE = sqrt(mean(error^2,na.rm=T)),
  #               MSA = 100*(exp(mean(abs(log(abundance/predicted_abundance)),na.rm=T))-1))
  #
  #
  #   return(list(
  #     all_mods=out %>% ungroup(),
  #     top_mods=tops %>% ungroup(),
  #     performance=perf
  #   ))
  # }


  #calculate rolling performance
  ## rolling_year_window<-15
  rp<-rolling_perf(results,dat,rolling_year_window,3,TY_ensemble,model_list)
  # rp$performance


  expect_true(!is.null(rp))

  # Assuming 'ens' is your data frame
  print(head(rp, n = 10))


  # Add more expectations as needed to check specific aspects of the results
  # Save the results to a CSV file
  #write.csv(ens, file = "tests/outputs/ensemble/ens.csv", row.names = FALSE)

  # You can add more test assertions here if needed

})

