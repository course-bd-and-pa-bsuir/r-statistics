library(TTR)
library(car)
library(lmtest)
library(nortest)
library(forecast)
#lines(lm(lbeer~t+t2+sin.t+cos.t)$fit,col=4)
bd=read.csv("Birth.csv",header=FALSE,sep=";")# прочитали
b.ts=ts(bd$V1,frequency=12, start=c(1946,1))# преобразовали в TS
is.ts(b.ts)
frequency(b.ts)
plot.ts(b.ts) #построили график ВР
monthplot(b.ts)
dts=decompose(b.ts)#  декомпозиция
plot(dts)     # график детерминированных составляющих 
dts$random
dts2=stl(b.ts,s.window="periodic")
plot(dts2)
res2=dts2$time.series[,3] 
#еще один график 
t=c(1:168)
plot(t,b.ts,type="l")
m1=lm(b.ts~t)
lines(t,fitted(m1),col="red")
# проверка на автокорреляцию
#acf(res2,lag.max=5) #ACF  остатков
acf(residuals(m1),lag.max=5) #ACF  остатков
acf(residuals(m1),lag.max=20) #ACF  остатков
#Box.test(res2,lag=5, type="Ljung-Box")
#тест на автокорр заданного лага Н0- автокорр. нет
#Бокса-Льюнга - стоит в тесте проверить каждый лаг
Box.test(residuals(m1),lag=1, type="Ljung-Box")
Box.test(residuals(m1),lag=3, type="Ljung-Box")
Box.test(residuals(m1),lag=5, type="Ljung-Box")
Box.test(residuals(m1),lag=20, type="Ljung-Box")
plot.ts(residuals(m1))
#Н0- автокорр. нет
durbinWatsonTest(residuals(m1),max.lag=5)
durbinWatsonTest(residuals(m1))
bgtest(m1,order=15,type="F")# работает только с моделью
pearson.test(residuals(m1))
#прогнозирование
fb.ts=forecast(b.ts,n=10)
plot(forecast(b.ts,n=10))
plot(forecast(residuals(m1)))
#скользящее среднее
sma=SMA(b.ts,n=3)
sma
summary(sma)
plot.ts(sma)
lines(b.ts,col="red")
#ХолтВинтерс - экспоненциальное сглаживание
hw=HoltWinters(b.ts)
plot(b.ts)
lines(hw$fitted[,1],col="blue")
#прогнозирование Холтера-Винтерса
predict(hw,n.ahead=12)
plot(b.ts,xlim=c(1946,1969))
lines(predict(hw,n.ahead=48),col=2)
#Экспоненциальное сглаживание без тренда и сезонности
hw=HoltWinters(b.ts,beta=FALSE,gamma=FALSE)
summary(hw)
hw$fitted
plot(hw)
hw$SSE # среднеквадратичная ошибка
fhw=forecast.HoltWinters(hw,h=10)
plot.forecast(fhw)
acf(fhw$residuals,lag.max=15)
acf(fhw$residuals,lag.max=26)
acf(fhw$residuals,lag.max=15)
Box.test(fhw$residuals,lag=20,type="Ljung-Box")
plot.ts(fhw$residuals)
pearson.test(fhw$residuals)
#точность прогноза
# точность MAPE 4% -- приемлемо 3-5%
accuracy(hw$fitted[,1],b.ts)
