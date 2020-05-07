setwd("/Users/xueyuanfan/Desktop/STAT/GR5261 Statistical mthd in Finance/project")
###load data
data<-read.csv("data_close.csv")
n<-dim(data)[1]
return<-data[2:n,]/data[1:(n-1),]-1

data<-read.csv("/Users/xueyuanfan/Desktop/STAT/GR5261 Statistical mthd in Finance/project/data_close.csv")

###numerical analysis of prices of assets
mean_data<-colMeans(data)
cov_data<-cov(data)
std_data<-sqrt(diag(cov_data))
cbind(mean_data,std_data)

###numerical anlaysis of returns 
mean_return<-colMeans(return)
cov_return<-cov(return)


std_return<-sqrt(diag(cov_return))


library(moments)
skew_return<-skewness(return)
kurt_return<-kurtosis(return)

SP500<-read.csv("^GSPC.csv")
SP500<-SP500[5]
SP_return<-SP500[2:1090,]/SP500[1:1089,]-1
beta_return<-cov(return,SP_return)/(sd(SP_return))^2
table1<-cbind(mean_return, std_return,skew_return, kurt_return,beta_return)
colnames(table1)[5]<-"beta"
table1



###graphical analysis
###price table
library(stringr)
name<-str_sub(colnames(data),1,-7)
length<-length(colnames(data))
par(mfrow=c(3,5))
for (i in 1:length){
  plot<-plot.ts(data[,i],main=paste(name[i],"Prices"), xlab="Time",ylab="Price")
  plot
}
 
###returns table
# Stock Returns Table
SP_return<-SP500[2:1090,]/SP500[1:1089,]-1
par(mfrow=c(2,4))
plot.ts(SP_return,ylim=c(-0.05,0.05),xlim=c(0,1080),main="SP500 Stock Returns")
plot.ts(return$MMM.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="MMM Stock Returns")
plot.ts(return$AXP.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="AXP Stock Returns")
plot.ts(return$AAPL.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="AAPL Stock Returns")
plot.ts(return$BA.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="BA Stock Returns")
plot.ts(return$CAT.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="CAT Stock Returns")
plot.ts(return$CVX.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="CVX Stock Returns")
plot.ts(return$CSCO.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="CSCO Stock Returns")
plot.ts(return$KO.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="KO Stock Returns")
plot.ts(return$JNJ.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="JNJ Stock Returns")
plot.ts(return$NKE.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="NKE Stock Returns")
plot.ts(return$PG.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="PG Stock Returns")
plot.ts(return$RTX.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="RTX Stock Returns")
plot.ts(return$UNH.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="UNH Stock Returns")
plot.ts(return$VZ.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="VZ Stock Returns")
plot.ts(return$DIS.Close,ylim=c(-0.05,0.05),xlim=c(0,1000),main="DIS Stock Returns")


###EQUITY CURVE
equity=lapply(return,function(data){
  exp(cumsum(log(data+1)))
})
par(mfrow=c(2,4))
for (i in 1:length(equity)){
  plot1<-plot.ts(equity[[i]],main=name[i],ylab="$",xlab="month")
  plot1
}
library(ggplot2)
geom_abline(mapping=aes())
####2.3 Outliers & Distribution
boxplot(return,las=2,main="Asset Returns Outlier")

# Heavy Tail (Excess Kurtosis = kurtosis coefficient - 3)
excess.kt<-kurt_data-3

#2.4 Sharpe's Ratio (using sample mean &sd in month)
rf_day=1.54/100/365 #daily risk free
sharp_ratio<-(mean_return-rf_day)/std_return

sort(sharp_ratio)
sharp_ratio<-data.frame(sharp_ratio)









