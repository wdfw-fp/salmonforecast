#optimization algorithm to find best stacking weights
find_stack_weights<-function(tau,metric,n,initial_weights,preds,obs){
  tweights<-initial_weights
  preds<-as.matrix(preds)
  obs<-obs
  tau=tau
  skill_list<-c(NULL)
  metric=metric
  cnt<-0
  # for(i in 1:n){
  while(cnt<n){
    pred_trs_ensemble<- preds %*% as.vector(tweights)
    Error <- pred_trs_ensemble - obs
    SE <- Error^2
    PE <- Error/obs
    APE <- abs(PE)
    LAR <- log(obs/pred_trs_ensemble)
    RMSE <- apply(SE,2,function(x){sqrt(mean(x))})
    MPE <- apply(PE,2,function(x){mean(x)})
    MAPE <- apply(APE,2,function(x){mean(x)})
    MSA <- apply(LAR,2,function(x){100*(exp(median(abs(x)))-1)})
    # if(i==1){
    if(cnt==0){
      skill=get(metric)
      weights=tweights
    }
    if(get(metric)<skill){
      skill=get(metric)
      weights=tweights
      cnt<-1
    } else {
      cnt<-cnt+1
    }
    skill_list<-c(skill_list,min(get(metric),skill))
    keep<-rbinom(1,prob=skill/get(metric),1)
    if(keep==1){tweights=tweights }else(tweights=weights)
    tweights = rdirichlet(n=1,alpha = tweights*tau+0.01)
  }
  results<-list(weights,skill,skill_list)
  return(results)
}
