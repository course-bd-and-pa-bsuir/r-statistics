library(TTR)
library(car)
library(lmtest)
library(nortest)
library(forecast)
brw = read.table("БЖД.csv", dec=",", header=TRUE, fill=FALSE)
brwM = read.table("БЖД-Минск.csv", dec=",", header=TRUE, fill=FALSE)
#brw.winter = factor(brw)

b.ts=ts(brwM$MINSK,frequency=4, start=c(2000,1))# преобразовали в TS
is.ts(b.ts)
frequency(b.ts)
plot.ts(b.ts) #построили график ВР
monthplot(b.ts)

# декомпозиция
dts=decompose(b.ts)
plot(dts)
dts$random

# Seasonal Decomposition of Time Series by Loess
dts2=stl(b.ts,s.window="periodic")
plot(dts2)
res2=dts2$time.series[,3] # get riminder

t=c(1:28)

# построение модели
mrw1 = lm(brwM$MINSK ~ I(1/t) + brwM$S1 + brwM$S2 +brwM$S3)
plot(t,b.ts,type="l")
lines(t,fitted(mrw1),col="red")
summary(mrw1)

mrw2 = lm(brwM$MINSK ~ I(1/t) + brwM$S1 + brwM$S3)
plot(t,b.ts,type="l")
lines(t,fitted(mrw2),col="red")
summary(mrw2)

# сравнение моделей
AIC(mrw2,mrw1) # mrw2 лучше

# проверяем значимость добавления фактора
anova(mrw2,mrw1) # непонятно

# показывает нормальность распр остатков
pearson.test(residuals(mrw1))
pearson.test(residuals(mrw2))

summary(mrw1)
summary(mrw2)

plot.ts(residuals(mrw1))
lines(t,residuals(mrw2),col="red")

#printsum(mrw1)
#printsum(mrw2)

# проверка на автокорреляцию
#acf(res2,lag.max=5) #ACF  остатков
acf(residuals(mrw1),lag.max=3) #ACF  остатков
acf(residuals(mrw2),lag.max=3) #ACF  остатков

acf(residuals(mrw1),lag.max=5) #ACF  остатков
acf(residuals(mrw2),lag.max=5) #ACF  остатков

#тест на автокорр заданного лага Н0- автокорр. нет
#Бокса-Льюнга - стоит в тесте проверить каждый лаг
Box.test(residuals(mrw1),lag=1, type="Ljung-Box") # есть автокорр остатков: p-value = 0.01335 < 0.05
Box.test(residuals(mrw2),lag=1, type="Ljung-Box")

Box.test(residuals(mrw1),lag=3, type="Ljung-Box") # есть автокорр остатков: p-value = 0.03016 < 0.05
Box.test(residuals(mrw2),lag=3, type="Ljung-Box")

#Н0- автокорр. нет
durbinWatsonTest(residuals(mrw1),max.lag=3)
durbinWatsonTest(residuals(mrw2),max.lag=3)

durbinWatsonTest(residuals(mrw1)) # p-value = 0.05
durbinWatsonTest(residuals(mrw2)) # p-value = 0.2166 - это гораздо лучше

bgtest(mrw1,order=1,type="F")# F-стат
bgtest(mrw2,order=1,type="F")# F-стат

bgtest(mrw1,order=2,type="F")# F-стат
bgtest(mrw2,order=2,type="F")# F-стат

bgtest(mrw1,order=1,type="Chisq")# По χ2
bgtest(mrw2,order=1,type="Chisq")# 

bgtest(mrw1,order=2,type="Chisq")# По χ2
bgtest(mrw2,order=2,type="Chisq")# 

# Д.б. p > 0.05 - гетероскедастичность отсутствует
# тут все норм
bptest(mrw1)
bptest(mrw2)

# малорелевантен
ncvTest(mrw1)
ncvTest(mrw2)

# мультиколлинеарность
# <10 эффекта мультиколлинеарности не наблюдается и 
vif(mrw1)
vif(mrw2)

#прогнозирование
fb.ts=forecast(b.ts,n=9)
plot(forecast(b.ts,n=8))
plot(forecast(residuals(mrw1)))
plot(forecast(residuals(mrw2)))

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
predict(hw,n.ahead=8)
plot(b.ts, xlim=c(2000,2015))
lines(predict(hw,n.ahead=24),col=2)
predict(hw,n.ahead=12)
lines(predict(hw,n.ahead=24),col="blue")

#Экспоненциальное сглаживание без тренда и сезонности
hw=HoltWinters(b.ts,beta=FALSE,gamma=FALSE)
summary(hw)
hw$fitted
plot(hw)

hw$SSE # среднеквадратичная ошибка

# прогнозирование
fhw=forecast.HoltWinters(hw,h=8)
plot.forecast(fhw)

# проверка на автокорреляцию остатков
acf(fhw$residuals,lag.max=7)
acf(fhw$residuals,lag.max=15)

Box.test(fhw$residuals,lag=20,type="Ljung-Box")
plot.ts(fhw$residuals)
pearson.test(fhw$residuals)

#точность прогноза
# точность MAPE 4% -- приемлемо 3-5%
accuracy(hw$fitted[,1],b.ts)
accuracy(fb.ts$fitted,b.ts)
