## Pre-process
library(tidyverse)
library(TTR)
library(janitor)
library(plotly)
library(caret)
library(lubridate)

raw=read.csv('all_info.csv',stringsAsFactors = FALSE)%>%
  clean_names()%>%
  mutate(date=as.Date(date))%>%
  rename(tri=tot_return_index_net_dvds)

## Select 2 etfs


put_label=function(index,k=7){
  roc=ROC(index,n=k,type = 'discrete')
  roc=roc[!is.na(roc)]
  roc=c(roc,rep(NA,k))
  return(roc)
}

get_tec=function(raw,etfs,k=7){
  
  etf1=raw%>%
    filter(etf==etfs[1])%>%
    select(date,tri)%>%
    rename(etf1=tri)
  etf2=raw%>%
    filter(etf==etfs[2])%>%
    select(date,tri)%>%
    rename(etf2=tri)
  
  pair=etf1%>%
    inner_join(etf2,by='date')%>%
    mutate(pair_index=etf1/etf2)%>%
    select(date,pair_index)%>%
    mutate(pair=paste0(etfs[1],'_',etfs[2]))
  
  
  
  
  
  pair_tecnical=pair%>%
    mutate(sma14     = SMA(pair_index,n=14)/pair_index,
           sma50     = SMA(pair_index,n=50)/pair_index,
           sma200    = SMA(pair_index,n=200)/pair_index,
           c14vs50   = sma14/sma50,
           c14vs200  = sma14/sma200,
           c50vs200  = sma50/sma200,
           ema       = EMA(pair_index,n=14)/pair_index,
           momentum  = momentum(pair_index,n=2),
           macd      = MACD(pair_index, nFast=12, nSlow=26,
                            nSig=9, maType=SMA)[,2],
           rsi       = RSI(pair_index, n=14),
           cci       = CCI(pair_index, n = 20),
           vhf       = VHF(pair_index, n = 28),
           label     = put_label(pair_index,k=k))%>%
    filter(!is.na(c50vs200),!is.na(label))%>%
    select(pair,date,label,pair_index,
           sma14,sma50,sma200,
           c14vs50,c50vs200,c14vs200,
           ema,momentum,
           macd,cci,vhf,rsi)%>%
    mutate(label=factor(ifelse(label>0,'act_1','act_2')))
  return(pair_tecnical)
}


get_model=function(training,met='knn'){
  
  fitControl <- trainControl(## 10-fold CV repeted 10 times
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    classProbs = TRUE)
  
  if(met=='gbm'){
    model <- train(label ~ ., data = training, 
                   method = met, 
                   trControl = fitControl,
                   preProcess=c("center", "scale"),
                   verbose=FALSE)
  }else{
    model <- train(label ~ ., data = training, 
                   method = met, 
                   trControl = fitControl,
                   preProcess=c("center", "scale"))
  }
  
  return(model)
}





get_year_model_and_pred=function(pair_tecnical,y=2019,yot=3,met='knn'){
  ini=as.Date('2010-01-01')
  year(ini)=y-yot
  cut=ini
  year(cut)=year(ini)+yot
  sco=ini
  year(sco)=year(ini)+yot+1
  
  training=pair_tecnical%>%
    filter(date<cut,date>=ini)%>%
    select(-date,-pair_index,-pair)
  
  test=pair_tecnical%>%
    filter(date>=cut,date<sco)
  
  model=get_model(training,met=met)
  
  prediction=test%>%
    mutate(expected=predict(model,.,type = "prob")[,1])%>%
    select(date,pair,label,expected)
  
  return(list(model=model,prediction=prediction))
}



etfs=c(
  'MCHI US Equity', #china
  'EWH US Equity', #hong kong
  'EWY US Equity', #korea
  'EWA US Equity', #Australia
  'INDA US Equity', #india
  'AAXJ US Equity' # Asia general
)




k=5

all_pairs=data.frame(combn(unique(etfs),2),stringsAsFactors = FALSE)

all_pairs=lapply(all_pairs, c)

all_tecs=lapply(all_pairs,get_tec,raw=raw,k=k)

pair_tecnical=do.call(rbind,all_tecs)


met='rf'
years=as.list(2015:2019)
strategy_and_model=lapply(years,
                get_year_model_and_pred,
                pair_tecnical=pair_tecnical,
                yot=5,
                met=met)
models=list()
strategy=list()
for (y in 1:length(years)){
  models[[y]]=strategy_and_model[[y]]$model
  strategy[[y]]=strategy_and_model[[y]]$prediction
}


strategy=do.call(rbind,strategy)%>%
  mutate(prob=expected,expected=factor(ifelse(expected>0.5,'act_1','act_2')))

get_importance=function(model){
  return(varImp(model, scale = TRUE)$importance)
}


if (met %in%c('rf','glm')){
  importances=do.call(cbind,lapply(models,get_importance))
  colnames(importances)=do.call(c,years)
  
  importances%>%
    mutate(feature=row.names(.))%>%
    gather(year,overall,-feature)%>%
    plot_ly(x=~feature,y=~overall,color = ~year,type='bar')
}
    
multiple_vote=function(prob,etf1,etf2){
  vote=etf1
  for (p in 1:length(prob)){
    pp=prob[p]
    if(pp>0.5){
      vote[p]=etf1[1] #normal
    }else{
      vote[p]=etf2[1]
    }
    if(abs(pp-0.5)<0.1){
      if(pp>0.5){
        vote[p]=etf2[1] #contrario
      }else{
        vote[p]=etf1[1] #contrario
      }
    }
  }
  return(vote)
}

portfolio=strategy%>%
  separate(pair,c('etf1','etf2'),'_')%>%
  mutate(vote=ifelse(prob>0.5,etf1,etf2),
         vote2=multiple_vote(prob,etf1,etf2))
portfolio_base=portfolio%>%
  select(vote,date,etf1,etf2)
portfolio_conf=portfolio%>%
  select(vote2,date,etf1,etf2)%>%
  rename(vote=vote2)


portfolio=portfolio_base%>%
  rbind(portfolio_conf)%>%
  count(date,vote)%>%
  spread(key=vote,value=n,fill=0)

portfolio_tidy=portfolio%>%
  gather(etf,w,-date)%>%
  group_by(date)%>%
  mutate(w=w/sum(w))




retornos_tidy_1a=raw%>%#el retorno del dia siguiente
  select(date,etf,tri)%>%
  group_by(etf)%>%
  mutate(ret=c(ROC(tri,n=1,type = 'discrete')[-1],NA))%>%
  ungroup()%>%
  select(date,etf,ret)

retorno_portafolio=portfolio_tidy%>%
  left_join(retornos_tidy_1a,by=c('etf','date'))%>%
  mutate(ret=ifelse(is.na(ret),0,ret))%>%
  group_by(date)%>%
  summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
  ungroup()%>%
  mutate(stat=cumprod(1+ret_stat),ew=cumprod(1+ret_ew))



portfolio_tidy%>%
  left_join(retornos_tidy_1a,by=c('etf','date'))%>%
  mutate(ret=ifelse(is.na(ret),0,ret))%>%
  group_by(date)%>%
  summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
  ungroup()%>%
  mutate(y=year(date))%>%
  group_by(y)%>%
  summarise(stat=prod(1+ret_stat)-1,ew=prod(1+ret_ew)-1)%>%
  ungroup()%>%
  gather(stat,ret,-y)%>%
  plot_ly(x=~y,y=~ret,color=~stat,type='bar')

retorno_portafolio%>%
  select(date,stat,ew)%>%
  gather(strategy,index,-date)%>%
  plot_ly(x=~date,y=~index,color=~strategy,mode='lines')






strategy%>%
  count(expected,label)%>%
  group_by(expected)%>%
  mutate(p=n/sum(n))%>%
  ungroup()%>%
  mutate(label=fct_rev(label))%>%
  ggplot(aes(label,expected,fill=p,label=round(p*100,3)))+
  geom_tile()+geom_text()



