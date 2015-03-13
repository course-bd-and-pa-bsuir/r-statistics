library(astsa)
library(tseries)
library(nortest)
library(moments)
par(mfrow=c(1,1))
# вывод временных интервалов
time(jj)
cycle(jj)

# Виден тренд и возрастание отклонения от тренда
plot(jj, ylab="Earnings per Share", main="J & J")
plot(jj, type="o", col="blue", lty="dashed")

# Ряд стационарен относительно мат. ожидания и нестационарен относительно дисперсии
# Вывод: ряд нестационарен
plot(diff(log(jj)), main="logged and diffed") 
plot(diff(jj), main="diffed")
acf(jj, 20) # видна корреляция последовательных наблюдений


# Формальная проверка на стационарность:
# - Расширеный тест Дики-Фуллера (H0 - ряд нестационарен)
adf.test(jj) # p > 0.05, ряд нестационарен

# - Тест Филипса-Перрона (H0 - ряд нестационарен)
pp.test(jj) # p < 0.05, ряд стационарен, противоречит другим тестам

# - Тест Квитковски-Филипса-Шмидта-Шина (H0 - ряд стационарен)
kpss.test(jj) # p < 0.05, ряд нестационарен


# Так как ряд нестационарен, его необходимо преобразовать
# - 1. можно продифференциировать ряд - перейти от абсолютных к приростным показателям
adf.test(diff(jj)) # p < 0.05, ряд стационарен
pp.test(diff(jj)) # p << 0.05, ряд стационарен
kpss.test(diff(jj)) # p > 0.05, ряд стационарен


# Построим для полученного стационарного ряда модель
fit <- arima(diff(jj),
      order=c(
        1, # Порядок авторегрессии 
        0, # Интегрированное скользящее среднее (сколько раз продифференциирован исходны ряд) 
        0  # Порядок скользящего среднего
        )
      )
fit
# Остатки распределены не нормально, модель неудовлетворительна
pearson.test(fit$residuals)$p.value


# Декомпозиция без сглаживания
# Видна сезонность и тренд
dts2 <- stl(jj, s.window="periodic")
plot(dts2)

# Выделим сезоны (кварталы) и тренд для модели
Q = factor(cycle(jj))
trend = time(jj)

reg = lm(log(jj)~trend+Q+0, na.action=NULL)
summary(reg)

pearson.test(residuals(reg)) # Распределение остатков нормальное
Box.test(residuals(fit),lag=4, type="Ljung-Box") # Автокорреляция
bgtest(reg, order=4, type="F") # Автокорреляция!

plot(residuals(reg))

# Итого: модель arima без учёта сезонности неудовлетворительна. 
# Модель lm с трендом и сезонность неудовлетворительна
# Необходимо уточнять модели

begin_trend = rep(0, 84)
begin_trend[0:10] = 1
end_trend = rep(0,84)
end_trend[64:84] = 1

reg = lm(log(jj)~trend+Q+0+begin_trend+end_trend, na.action=NULL)
summary(reg)

pearson.test(residuals(reg))
Box.test(residuals(fit),lag=4, type="Ljung-Box")
bgtest(reg, order=4, type="F")

bptest(fit)
gqtest(fit)

plot(residuals(reg))
plot(fitted(reg))
plot(log(jj))
lines(fitted(reg), col="red")
