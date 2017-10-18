
library(caschrono)

library(TTR)

library(fGarch)

library(rugarch)

library(forecast)

library(TSA)



#Arima

xy.acfb(crix$price,numer=FALSE)

adf.test(crix$price)

#Augmented Dickey-Fuller Test:not stationary



#1)return

r=diff(log(crix$price))*100

plot(r,type="b")

abline(h = 0)

plot(r,type="l")



#2)Model Specification ARIMA(p,d,q)

#ADF test-H0:unit root H1:no unit root(test for stationarity)

adf.test(r)

#p-value=0.27,not stationary.

dr=diff(r)

plot(dr,type="b")

abline(h = 0)

adf.test(dr)

#p-value=0.01,stationary.(d=1)



#3)Parameter Estimation

#estimation of p and q



a.fin1=auto.arima(dr)

summary(a.fin1)

#ARMA(0,0) therefore r fits ARIMA(0,1,0)

a.fin2=arima(r,order=c(0,1,0))

summary(a.fin2)

help("forecast.Arima")

f=forecast(a.fin2,h=3,level=c(99.5))

acf(f$residuals,lag.max = 20)

Box.test(f$residuals,lag=20,type='Ljung-Box')

#the residuals follow Gaussian distribution

plot.ts(f$residuals)



#4)some evidence to GARCH model

#get ACF and PACF of the residuals

xy.acfb(residuals(a.fin2),numer=FALSE)

xy.acfb((residuals(a.fin2))^2,numer=FALSE)+
  
  xy.acfb(abs(residuals(a.fin2)),numer=FALSE)



#get the Conditional heteroskedasticity test

McLeod.Li.test(y=residuals(a.fin2))

#p-values are all included in the test, it formally shows strong evidence for ARCH in this data.



#**Normality of the Residuals

qqnorm(residuals(a.fin2))

qqline(residuals(a.fin2))

shapiro.test(residuals(a.fin2))

#The QQ plot suggest that the distribution of returns may have a tail thicker that of a 

#normal distribution and maybe somewhat skewed to the right

#p-value<0.05 reject the normality hypothesis



g1=garchFit(~garch(1,1),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)

summary(g1)

g2=garchFit(~garch(1,2),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)

summary(g2) 

g3=garchFit(~garch(2,1),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)

summary(g3)

g4=garchFit(~garch(2,2),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)

summary(g4)

#The best one is Garch(1,1) model which has the smallest AIC.