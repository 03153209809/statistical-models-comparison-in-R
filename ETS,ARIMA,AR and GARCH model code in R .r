library(forecast) 
library(tseries)
#library("tseries", lib.loc="~/R/win-library/3.5")

file <- read.csv(file="/home/abc/Documents/my-data/progrium-load-15mint.csv")
#file <- read.csv(file="/home/abc/Documents/my-data/qlpine-workload-15mint.csv")
rowindex <- c(140,240,340,440,540,640,740,850)
h<-4
i<-8
# rowindex <- c(160,320)
AR <- c()
ETS <- c()
ARIMA <- c()
#GARCH <- c()
for(i in 1:length(rowindex))
{
  train.data <- file$value[1:rowindex[i]];
  test.data <- file$value[(rowindex[i]+1):(rowindex[i]+h)];
  
  arima.model <- auto.arima(train.data,trace=TRUE);
  autoarima_pred <- forecast(arima.model,h)$mean;
  arima_accuracy <- accuracy(autoarima_pred,test.data);
  print(train.data)
  #print(arima_accuracy[2])
  ARIMA <- append(ARIMA,arima_accuracy[2])
  
  ar.model<- ar(train.data);
  ar_pred <- predict(ar.model, n.ahead=12);
  ar_accuracy <- accuracy(ar_pred$pred[1:h],test.data);
  AR <- append(AR,ar_accuracy[2])
  
  ets.model <- ets(train.data,na.action=na.interp);
  ets_pred <- forecast(ets.model,h)$mean;
  ets_accuracy <- accuracy(ets_pred,test.data);
  ETS <- append(ETS,ets_accuracy[2])
  
  garch.model <- garch(train.data, order = c(1,2))
  garch_pred <- predict(garch.model,test.data,genuine = FALSE)
  gar_accuracy <- accuracy(garch_pred[1:4],test.data)
  GARCH <- append(GARCH,gar_accuracy[2])
  
  print(arima_accuracy)
  print(ar_accuracy)
  print(ets_accuracy)
  print(gar_accuracy)
}

#pdf(test.pdf)
counts <- rbind(AR,ARIMA,ETS,GARCH)
print(counts)
colnames(counts)<-c(140,240,340,440,540,640,740,850)
# Expand right side of clipping rect to make room for the legend
par(xpd=T,mar=par()$mar+c(0,0,0,4))
barplot(counts,xlab="Length of time window used for model training",ylab="MAPE[%]",col=c("blue","red","green"),ylim=c(0,25),beside=TRUE,font.lab=2,font.axis=2)
legend(28,20,c("AR","ARIMA","ETS"),fill=c("blue","red","green"),text.font=2)
#barplot(counts,main="Time series prediction mape",xlab="Method")
#dev.off()





