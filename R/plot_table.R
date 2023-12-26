# function to make plots and tables
library(knitr)
library(kableExtra)
library(ggplot2)
plot_table<-function(
    rp,
    ens

  ){

  for_skill<-
    rp$performance %>% mutate(model="Best_individual") %>% bind_rows(
      ens$forecast_skill %>%filter(grepl("weight",model))
    )  %>%
    arrange(MAPE)




  Table2<-for_skill%>% mutate(model=sub("_"," ",model))%>% rename(Model=model)%>%
    kbl(caption = paste0("Table 2. One step ahead individual and ensemble model performance"),digits =2)%>%
    kable_classic(full_width = F, html_font = "Cambria")





  Table3<-
    # results$forecasts%>%
    # filter(year==max(year))%>%
    rp$top_mods %>% filter(rank==1) %>% arrange(year) %>%
    dplyr::select(year,model_name,predicted_abundance,`Lo 50`,`Hi 50`,`Lo 95`,`Hi 95`) %>% #,Stacking_weight)%>%
    rename(Year=year,Model=model_name,`Predicted abundance` =predicted_abundance) %>%
    kbl(caption = paste0("Table 3. Individual models which performed the best in",stack_metric," based on a",rolling_year_window, "year retrsopective analysis, and their forecasts."),digits =2)%>%
    kable_classic(full_width = F, html_font = "Cambria")



  best_model<-for_skill$model[1]


  results_best<-rp$top_mods %>% filter(rank==1) %>% mutate(model="Best_individual") %>% bind_rows(
    ens$ensembles %>% left_join(dat %>% dplyr::select(year,abundance)) %>% mutate(model_name=model)
  ) %>% filter(model==best_model) %>%
    dplyr::select(year,model_name,abundance,
                  predicted_abundance,"Lo 50",
                  "Hi 50","Lo 95","Hi 95") %>%
    mutate(error=predicted_abundance-abundance,
           pct_errpr=error/abundance)






  Table4<-results_best%>%
    rename(Year=year,Model=model_name,Abundance=abundance,`Predicted abundance`=predicted_abundance,`% error`=pct_errpr) %>%
    # dplyr::select(!c("train_test","Stacking_weight","model_name") & !c(covariates[covariates%in%names(results_best)]))%>%
    kbl(caption = paste0("Table 4. One-year-ahead forecasts based on the ",best_model," model."),digits =2)%>%
    kable_classic(full_width = F, html_font = "Cambria")


  Figure1<-ggplot(results_best,aes(x=year,y=predicted_abundance))+
    geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`),color=NA,alpha=0.5,fill = "cadetblue")+
    geom_ribbon(aes(ymin=`Lo 50`,ymax=`Hi 50`),color=NA,alpha=0.5,fill = "cadetblue")+
    geom_line()+
    geom_point(aes(x=year,y=abundance))+
    ylim(0,NA)+
    scale_x_continuous(breaks=unique(results_best$year))+ylab("Ocean abundance")+xlab("")



  Figure2<-ggplot(rp$top_mods %>% filter(rank==1) %>% mutate(model="Best_individual")%>% bind_rows(
    ens$ensembles%>% left_join(dat %>% dplyr::select(year,abundance)) %>% mutate(model_name=model)),aes(x=year,y=predicted_abundance,col=model))+
  geom_line()+
  geom_point(aes(x=year,y=abundance),color="black")+
  ylim(0,NA)+
  scale_x_continuous(breaks=unique(results_best$year))+
  theme(legend.position = "none")+
  ylab("Ocean abundance")+xlab("")+
  theme(legend.key.size=unit(.15,'cm'),legend.position = "top")


  Figure3<-ggplot((results_best ) %>% mutate(Model=sub("_"," ",model_name)) %>%  mutate(pct_error=log10(((predicted_abundance-abundance)/abundance)+1)) %>% filter(year<2023),
                  aes(x=year,y=pct_error,color=Model))+geom_hline(yintercept=0)+geom_line(lwd=1.25)+scale_color_manual(values=c("darkblue","darkred"))+xlab("")+ylab(expression(paste(log[10], "(% error +1)")))


return(
  list(
    Table2=Table2,
    Table3=Table3,
    Table4=Table4,
    Figure1=Figure1,
    Figure2=Figure2
  )
)

}
