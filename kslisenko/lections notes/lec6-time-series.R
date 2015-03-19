## анализ временных рядов
birth <- read.table("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/Birth.csv", quote="\"")
## значения ряда должны быть измерены через равные промежутки времени
## пропуски или замены местами не допускаются
## уровень ряда - каждое конкретное значение (хотя ряд может быть представлен и в виде разностей, индексов, относительных величинах и т.д.)
## ряды могут быть многомерными (набор показатель для одной точки) и одномерными (один показатель)
## тренд = детерминированная составляющая + сезонная + ошибки
install.packages(c("TTR", "forecast"))

library(TTR)
library(car)
library(lmtest)
library(nortest)
library(forecast)

## преобразование в TS - для временных рядов создаём отдельный объект
## данные ежемесячные, поэтому frequency = 12
timeSeries = ts(birth$V1, frequency = 12, start = c(1946, 1))
is.ts(timeSeries)
frequency(timeSeries)
plot(timeSeries)
monthplot(timeSeries)

## разложение ряда на детерминированные составляющие
dts = decompose(timeSeries)
plot(dts) ## график детерминированных составляющих: тренд, сезонная, ошибки
dts$random
dts$seasonal ## поправочный коэффициент для каждого месяца
dts$trend
plot(dts$trend) ## график тренда

## аналогичный результат с помощью функции stl (но без сглаживания)
dts2 = stl(timeSeries, s.window = "periodic")
plot(dts2)

## подпишем внизу время наблюдения и добавим линию регрессии
t = c(1:168)
plot(t, timeSeries, type="l")
m1 = lm(timeSeries ~ t)
lines(t, fitted(m1), col="red")

## проверка на автокорреляцию
acf(residuals(m1), lag.max = 20) ## автокорреляция остатков
## тест на автокорреляцию, автокорреляция есть
Box.test(residuals(m1), lag=20, type="Ljung-Box")
durbinWatsonTest(residuals(m1))
bgtest(m1, order=15, type="F") ## работает только с моделью
pearson.test(residuals(m1))
## прогнозирование
forecastTs = forecast(timeSeries, n=10)
plot(forecastTs)
plot(forecast(dts2))
## скользящее среднее
sma3 = SMA(timeSeries, n=3) ## ширина окна - 3 наблюдения
summary(sma3)
plot(sma3)
lines(timeSeries, col = "red")
sma5 = SMA(timeSeries, n=5) ## ширина окна - 3 наблюдения
plot(sma5)
sma20 = SMA(timeSeries, n=20) ## ширина окна - 3 наблюдения
plot(sma20)
## Экспонентциальное сглаживание
hw = HoltWinters(timeSeries)
plot(hw)

## Экспонентциальное сглаживание без тренда и сезонности
hwNoTrendSeason = HoltWinters(timeSeries, beta = FALSE, gamma = FALSE)
summary(hwNoTrendSeason)
hwNoTrendSeason$fitted
plot(hwNoTrendSeason)
forecastHW = forecast.HoltWinters(hwNoTrendSeason, h=10)
plot.forecast(forecastHW)
acf(forecastHW$residuals, lag.max = 15)
Box.test(forecastHW$residuals, lag=20, type="Ljung-Box")
plot.ts(forecastHW$residuals)
pearson.test(forecastHW$residuals)
## точность прогноза
accuracy(hw$fitted[,1], timeSeries)

## Задание: датасет БЖД, смоделировать сезонность с помощью фиктивных переменных
bgd <- read.csv("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/bgd.csv", sep=";")




View(bgd)
bgd$MINSK ## один временной ряд для Минска
timeSeriesBGD = ts(bgd$MINSK, frequency = 1)
plot(timeSeriesBGD)
dts = decompose(timeSeriesBGD)
plot(dts)
## построим регрессию
t = c(1:28)
plot(t, timeSeriesBGD, type="l")
regBGD = lm(timeSeriesBGD ~ t + bgd$season1 + bgd$season2 + bgd$season3 + bgd$trend)
summary(regBGD)

dts = decompose(timeSeriesBGD)
plot(dts)

## временной ряд и его регрессия
plot(timeSeriesBGD)
lines(t, fitted(regBGD), col="red")
acf(residuals(regBGD), lag.max = 10)  ## нет автокорреляции
Box.test(residuals(regBGD), lag=5, type="Ljung-Box") ## нет автокорреляции
bgtest(regBGD,order=1) ## нет автокорреляции
durbinWatsonTest(regBGD) ## только этот тест портит малину :(
pearson.test(residuals(regBGD)) ## нет автокорреляции
## прогнозирование
forecastTsBGD = forecast(timeSeriesBGD, n=10)
plot(forecastTsBGD)

