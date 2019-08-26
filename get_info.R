library(Rblpapi)
library(dplyr)



data=read.csv("info.csv",stringsAsFactors = FALSE,sep = ",")

t1=data$Ticker


t1=unique(t1)
con=blpConnect()


a= bdh(t1,c("PX_LAST","PX_VOLUME","TOT_RETURN_INDEX_NET_DVDS"),start.date =as.Date("2019-01-01") )
num.secs=length(a)
Names=names(a)
df=data.frame()
problemas=c("problemas")
for(i in 1:num.secs){
  mini=a[[i]]
  
  if(nrow(mini)>0){
    df=rbind(df,mini)
  }else{
    problemas=c(problemas,Names[i])
  }
  
}

write.csv(df,'all_info.csv')
blpDisconnect(con)