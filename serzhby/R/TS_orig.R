library(TTR)
library(car)
library(lmtest)
library(nortest)
library(forecast)
lines(lm(lbeer~t+t2+sin.t+cos.t)$fit,col=4)
bd=read.csv("d:\\!work\\R-statistica\\“ренинг\\Birth.csv",header=FALSE,sep=";")# прочитали
b.ts=ts(bd$V1,frequency=12, start=c(1946,1))# преобразовали в TS
is.ts(b.ts)
frequency(b.ts)
plot.ts(b.ts) #построили график ¬–
monthplot(b.ts)
dts=decompose(b.ts)#  декомпозици€
plot(dts)     # график детерминированных составл€ющих 
dts$random
dts2=stl(b.ts,s.window="periodic")
plot(dts2)
res2=dts2$time.series[,3] 
#еще один график 
t=c(1:168)
plot(t,b.ts,type="l")
m1=lm(b.ts~t)
lines(t,fitted(m1),col="red")
# проверка на автокоррел€цию
acf(res2,lag.max=5) #ACF  остатков
Box.test(res2,lag=5, type="Ljung-Box")
plot.ts(res2)
durbinWatsonTest(res2,max.lag=5)
bgtest(m3,order=15,type="F")# работает только с моделью
pearson.test(res2)
#прогнозирование
fb.ts=forecast(b.ts,n=10)
plot(forecast(dts2))
#скольз€щее среднее
sma=SMA(b.ts,n=3)
sma
summary(sma)
plot.ts(sma)
lines(b.ts,col="red")
#’олт¬интерс
hw=HoltWinters(b.ts)
plot(b.ts)
lines(hw$fitted[,1],col="blue")
#прогнозирование
predict(hw,n.ahead=12)
plot(b.ts,xlim=c(1946,1969))
lines(predict(hw,n.ahead=48),col=2)
#Ёкспоненциальное сглаживание без тренда и сезонности
hw=HoltWinters(b.ts,beta=FALSE,gamma=FALSE)
summary(hw)
hw$fitted
plot(hw)
hw$SSE
fhw=forecast.HoltWinters(hw,h=10)
plot.forecast(fhw)
acf(fhw$residuals,lag.max=15)
acf(hw$residuals,lag.max=15)
Box.test(fhw$residuals,lag=20,type="Ljung-Box")
plot.ts(fhw$residuals)
pearson.test(fhw$residuals)
#точность прогноза
accuracy(hw$fitted[,1],b.ts)