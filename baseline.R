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
  roc=ROC(index,n=7,type = 'discrete')
  roc=roc[!is.na(roc)]
  roc=c(roc,rep(NA,k))
  return(roc)
}

get_tec=function(raw,etfs){
  
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
           label     = put_label(pair_index))%>%
    filter(!is.na(c50vs200),!is.na(label))%>%
    mutate(label=factor(ifelse(label>0,1,0)))
    return(pair_tecnical)
}


get_test_predictions=function(training,test,met='knn'){

  fitControl <- trainControl(## 10-fold CV repeted 10 times
    method = "repeatedcv",
    number = 5,
    repeats = 10)

  model <- train(label ~ ., data = training, 
                  method = met, 
                  trControl = fitControl,
                  preProcess=c("center", "scale"))


  test_pred=test%>%
    mutate(expected=predict(model,.))%>%
    select(date,pair,label,expected)
  return(test_pred)
}





get_year_strategy=function(pair_tecnical,y=2019,yot=3,met='knn'){
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
  
  
  strategy=get_test_predictions(training ,test,met='knn')
  
  return(strategy)
}


etfs=c('EWP US Equity',
       'EWI US Equity',
       'EWD US Equity',
       'EWU US Equity',
       'EWG US Equity',
       'EWQ US Equity',
       'EWN US Equity',
       'EWL US Equity',
       'IEV US Equity',
       'EZU US Equity')

all_pairs=data.frame(combn(etfs,2),stringsAsFactors = FALSE)

all_pairs=lapply(all_pairs, c)

all_tecs=lapply(all_pairs,get_tec,raw=raw)

pair_tecnical=do.call(rbind,all_tecs)



stat19=get_year_strategy(pair_tecnical,y=2019,yot=5,met='knn')

stat18=get_year_strategy(pair_tecnical,y=2018,yot=5,met='knn')
    
strategy=rbind(stat19,stat18)
k=length(unique(strategy$pair))

portfolio=strategy%>%
  separate(pair,c('etf1','etf2'),'_')%>%
  mutate(vote=ifelse(expected=='1',etf1,etf2))%>%
  count(date,vote)%>%
  mutate(n=n/(k))%>%
  spread(key=vote,value=n,fill=0)

portfolio_tidy=portfolio%>%
  gather(etf,w,-date)

retornos_tidy_1a=raw%>%#el retorno del dia siguiente
  select(date,etf,tri)%>%
  group_by(etf)%>%
  mutate(ret=c(ROC(tri,n=1,type = 'discrete')[-1],NA))%>%
  ungroup()%>%
  select(date,etf,ret)

retorno_portafolio=portfolio_tidy%>%
  left_join(retornos_tidy_1a,by=c('etf','date'))%>%
  group_by(date)%>%
  summarise(ret_stat=sum(w*ret),ret_ew=mean(ret))%>%
  ungroup()%>%
  mutate(stat=cumprod(1+ret_stat),ew=cumprod(1+ret_ew))

retorno_portafolio%>%
  select(date,stat,ew)%>%
  gather(strategy,index,-date)%>%
  plot_ly(x=~date,y=~index,color=~strategy,mode='lines')


  
