install.packages(c("tseries", "forecast", "zoo", "FinTS", "rugarch"))
library("tseries")
library("zoo")
library("forecast")
library("FinTS")
library("rugarch")
setwd("C:/Users/manal/Desktop/FTS project")
price=as.data.frame(vixcurrent$VIXClose)
date = as.Date(vixcurrent$Date, "%m/%d/%Y")
plot(date,vixcurrent$VIXClose)
N=length(price[,1])
VIX.returns=100*(log(price[2:N,])-log(price[1:(N-1),]))
VIX.returns
VIX.returns=c(VIX.returns,1)
plot(date,VIX.returns)
VaR5n=NULL
logret=VIX.returns
for (i in 1:59)
{
model=garchFit(~garch(1,1),data=logret,trace=F,cond.dist='norm')
model
x=as.data.frame(predict(model,5));
x
VaR5n =append(VaR5n,x$meanForecast[1]- qnorm(1-0.05)*(x$meanError[1]))
VaR5n
logret=logret[-1]
}
VaR5n
meanVaR=NULL
for (i in 1:59)
{
  meanVaR=append(meanVaR,mean(VaR5n[1:i]))
}
meanVaR
summary(meanVaR)
Gaussian_DCC=NULL
for (i in 1:59)
{
  Gaussian_DCC=append(Gaussian_DCC,min(meanVaR[i],VaR5n[i]))
  
}
Gaussian_DCC
norm_Gaussian_DCC=normalized(Gaussian_DCC)
summary(norm_Gaussian_DCC)
#Calculating Daily Capital Charge Using Forecasted VaR Calculated by GARCH(1,1)DCC=max(-VaR5n60day,-VaR5n)

#prediction of VaR using GARCH(1,1)-student t distribution
tdist_logret=VIX.returns
tdist_VaR5n=NULL
for (i in 1:59)
{
tdist_model=garchFit(~garch(1,1),data=tdist_logret,trace=F,cond.dist=c("std"))
x=as.data.frame(predict(tdist_model,5))
tdist_VaR5n =  append(tdist_VaR5n,x$meanForecast[1]- qnorm(1-0.05)*(x$meanError[1]))
#VaR5nt
tdist_logret=tdist_logret[-1]
}
tdist_VaR5n

tdist_meanVaR=NULL
for (i in 1:59)
{
  tdist_meanVaR=append(tdist_meanVaR,mean(tdist_VaR5n[1:i]))
}
tdist_meanVaR
summary(tdist_meanVaR)
#Calculating Daily Capital Charge Using Forecasted VaR Calculated by GARCH(1,1)DCC=max(-VaR5n60day,-VaR5n)
tdist_DCC=NULL
for (i in 1:59)
{
  tdist_DCC=append(tdist_DCC,min(tdist_meanVaR[i],tdist_VaR5n[i]))
  
}
tdist_DCC
norm_tdist_DCC=normalized(tdist_DCC)
summary(norm_tdist_DCC)

gjr_logret=VIX.returns
spec.gjrGARCH = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model="norm")
gjrGARCH <- ugarchfit(gjr_logret, spec=spec.gjrGARCH)
gjrGARCH
spec2=spec.gjrGARCH
setfixed(spec2)=as.list(coef(gjrGARCH))
filt=ugarchfilter(spec2,gjr_logret)
gjr_VaR=fitted(filt)+sigma(filt)+qdist("norm",p=0.05,mu=0,sigma=1,skew=coef(gjrGARCH)["skew"],shape=coef(gjrGARCH)["shape"])
gjr_VaR[(length(gjr_VaR) -60):length(gjr_VaR)]
gjr_meanVaR=NULL
for (i in 1:59)
{
  gjr_meanVaR=append(gjr_meanVaR,mean(gjr_VaR[1:i]))
}
gjr_meanVaR
summary(gjr_meanVaR)
gjrGaussian_DCC=NULL
for (i in 1:59)
{
  gjrGaussian_DCC=append(gjrGaussian_DCC,min(gjr_meanVaR[i],gjr_VaR[i]))
  
}
gjrGaussian_DCC
norm_gjrGaussian_DCC=normalized(gjrGaussian_DCC)
summary(norm_gjrGaussian_DCC)
gjr_logret=VIX.returns
spec.gjrGARCH = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model="std")
gjrGARCH <- ugarchfit(gjr_logret, spec=spec.gjrGARCH)
gjrGARCH
spec2=spec.gjrGARCH
setfixed(spec2)=as.list(coef(gjrGARCH))
filt=ugarchfilter(spec2,gjr_logret)
fitted(filt)
sigma(filt)
qdist("sstd",p=0.05,mu=0,sigma=1,skew=coef(gjrGARCH)["skew"],shape=coef(gjrGARCH)["shape"])
gjr_tVaR=fitted(filt)+sigma(filt)+qdist("sstd",p=0.05,mu=0,sigma=1,skew=coef(gjrGARCH)["skew"],shape=coef(gjrGARCH)["shape"])
gjr_tVaR[(length(gjr_VaR) -60):length(gjr_VaR)]
gjr_tmeanVaR=NULL
for (i in 1:59)
{
  gjr_tmeanVaR=append(gjr_tmeanVaR,mean(gjr_tVaR[1:i]))
}
gjr_tmeanVaR
summary(gjr_tmeanVaR)
gjrtdist_DCC=NULL
for (i in 1:59)
{
  gjrtdist_DCC=append(gjrtdist_DCC,min(gjr_tmeanVaR[i],gjr_VaR[i]))
  
}
gjrtdist_DCC
norm_gjrtdist_DCC=normalized(gjrtdist_DCC)
summary(norm_gjrtdist_DCC)

normalized=function(x)
{
  z=(x-mean(x))/stdev(x)
  return (z)
}
par(mfrow=c(2,2))
plot(ecdf(norm_Gaussian_DCC), col = "red",main="GARCH-Gaussian Vs GARCH-student t")
lines(ecdf(norm_tdist_DCC), col = "blue")


plot(ecdf(norm_gjrGaussian_DCC), col = "red" ,main="GJR-Gaussian Vs GJR-student t")
lines(ecdf(norm_gjrtdist_DCC), col = "blue")

plot(ecdf(norm_tdist_DCC), col = "red" , main="GARCH-student t Vs GJR-student t")
lines(ecdf(norm_gjrtdist_DCC), col = "blue")

plot(ecdf(norm_Gaussian_DCC), col = "red", main="GARCH-Gaussian Vs GJR-Gaussian")
lines(ecdf(norm_gjrGaussian_DCC), col = "blue")


