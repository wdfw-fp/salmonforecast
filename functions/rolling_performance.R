

rolling_perf<-function(one_aheads,series,roll_years=15,mod_include=5){
  
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
