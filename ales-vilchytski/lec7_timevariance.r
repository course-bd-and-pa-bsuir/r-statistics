# Лекция 7 - временные ряды

library(TTR)
library(car)
library(lmtest)
library(nortest)
library(forecast)

par(mfrow=c(1,1))
bd <- read.csv("data/Birth.csv",header=FALSE,sep=";")

b.ts <- ts(bd$V1, frequency=12, start=c(1946, 1)) # преобразование в Time Series Object
is.ts(b.ts)
frequency(b.ts)
plot.ts(b.ts)

monthplot(b.ts)
# декомпозиция
# random - случайная составляющая (ошибки)
# type - тип модели (аддитивная, мультипликативная)
dts <- decompose(b.ts)
plot(dts) # график детерминированных составляющих 

# декомпозиция без сглаживания
dts2 <- stl(b.ts, s.window="periodic")
plot(dts2)

# График с зависимостью по времени
t <- c(1:168)
plot(t, b.ts, type="l")
m1 <- lm(b.ts~t)
lines(t, fitted(m1), col="red")


# Проверка на автокоррел¤цию
# Графическая интерпретация
acf(residuals(m1), lag.max=5) #ACF остатков
acf(residuals(m1), lag.max=20)
# Тест Бокса-Юнга
# p-value < 0.05 - есть автокорелляция
Box.test(residuals(m1),lag=20, type="Ljung-Box")
plot.ts(residuals(m1))
# Тест Дарбина-Уотсона до пятого порядка
# Значение статистики -> 2 говорит об отсутствии АК четвёртого порядка, в остальных случаях - присутствие АК
durbinWatsonTest(residuals(m1), max.lag=5)
bgtest(m1, order=15, type="F")
pearson.test(residuals(m1)) # p > 0.05, остатки имеют нормальное распределение


# Скользящее среднее
sma <- SMA(b.ts, n=3) # окно по три элемента
summary(sma)
plot.ts(sma)
# lines(b.ts, col="red")


# Модель Холта-Уинтерса
hw <- HoltWinters(b.ts)
plot(hw$fitted[,1])
# lines(b.ts, col="red")

#прогнозирование
predict(hw, n.ahead=12)
plot(b.ts, xlim=c(1946,1969))
lines(predict(hw, n.ahead=48), col=2)

# Экспоненциальное сглаживание без тренда и сезонности
# SSE - среднеквадратичная ошибка
hw <- HoltWinters(b.ts, beta=FALSE, gamma=FALSE)
summary(hw)
plot(hw)
fhw <- forecast.HoltWinters(hw, h=10)
plot.forecast(fhw)

acf(fhw$residuals, lag.max=15)
acf(residuals(hw), lag.max=15)
# p-value < 0.05 - наличие АК
Box.test(fhw$residuals, lag=20, type="Ljung-Box")

pearson.test(fhw$residuals)
plot.ts(fhw$residuals)

# Точность прогноза
# Первый аргумент - модель или прогнозный ряд, второй - исходный ряд
# MAPE - средний абсолютный процент ошибки (д.б. в пределах 3-5%)
accuracy(hw$fitted[,1], b.ts)
