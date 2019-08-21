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



etfs=c('EWD US Equity','EWP US Equity')

pair_tecnical=get_tec(raw,etfs)

etfs=c('EWI US Equity','EWD US Equity')

pair_tecnical2=get_tec(raw,etfs)

etfs=c('EWP US Equity','EWI US Equity')

pair_tecnical3=get_tec(raw,etfs)

pair_tecnical=pair_tecnical%>%
  rbind(pair_tecnical2)%>%
  rbind(pair_tecnical3)


  
