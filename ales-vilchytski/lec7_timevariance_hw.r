# Задание:
# Для данных о БЧ построить модель множественной регрессии. Необходимо выбрать
# один временной ряд и учесть сезонность в модели.

library(TTR)
library(car)
library(lmtest)
library(nortest)
library(forecast)

data <- read.csv('data/bzhd.csv', header=TRUE)
data$SEASON = factor(data$SEASON)

# Проверка корректности модели
check.fit <- function(fit, lag = 4, p.value = 0.05) {
  res <- c()
  
  # Если p > 0.05 - распределение нормальное
  res$NORM_JQ = jarque.test(residuals(fit))$p.value
  res$NORM_PE = pearson.test(residuals(fit))$p.value
  res$NORM_OK = res$NORM_JQ > p.value && res$NORM_PE > p.value
  
  # Автокорреляция отсутствует при p > 0.05
  res$AC_BOX = Box.test(residuals(fit),lag=lag, type="Ljung-Box")$p.value
  res$AC_BG = bgtest(fit, order=lag, type="F")$p.value
  res$AC_OK = res$AC_BOX > p.value && res$AC_BG > p.value
  
  # Модель гомоскедастична при p > 0.05
  res$GS_BP = bptest(fit)$p.value
  res$GS_GQ = gqtest(fit)$p.value
  res$GS_OK = res$GS_BP > p.value && res$GS_GQ > p.value
  
  res$ALL_OK = res$NORM_OK && res$AC_OK && res$GS_OK
  
  # Визуализация:
  #  - график остатков
  mfrow <- par("mfrow")
  par(mfrow=c(1,3))
  plot(residuals(fit), type="b")
  
  # - гистограмма с линией нормального распределения
  ress <- residuals(fit)
  h <- hist(ress, breaks=20) # гистограмма остатков
  xfit<-seq(min(ress),max(ress),length=40) 
  yfit<-dnorm(xfit,mean=mean(ress),sd=sd(ress)) 
  yfit <- yfit*diff(h$mids[1:2])*length(ress) 
  lines(xfit, yfit, col="black", lwd=2)
  
  # - график распределения теоретического и реального норм. распределения
  qqnorm(residuals(fit))
  qqline(residuals(fit))
  par(mfrow=mfrow)
  
  return(res)
}

# Временной рядик
b.ts <- ts(data$MINSK, frequency=4, start=c(2000, 1)) # преобразование в Time Series Object
plot.ts(b.ts)

dts <- decompose(log(b.ts))
plot(dts) # график детерминированных составляющих 

# Моделька
fit <- lm(log(MINSK)~(I(1/N)+SEASON), data=data)
summary(fit)

check.fit(fit, 4)

# Дополнительные значения SEASON для прогнозирования
newdata <- read.csv('data/bzhd_predict.csv', header=TRUE)
newdata$SEASON <- factor(newdata$SEASON)

# Прогнозирование ???
plot(c(log(b.ts), ts(predict.lm(fit, newdata, n.ahead=4))), type="b")
lines(c(28,28), c(0, 2000), col='red')
accuracy(fitted(fit), b.ts)

# Другой метод, но точность похуже
fb.ts = forecast(b.ts, n=4)
accuracy(fitted(fit), b.ts)
accuracy(fb.ts$fitted, b.ts)
