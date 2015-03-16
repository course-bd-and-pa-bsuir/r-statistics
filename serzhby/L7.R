setwd('~/code/R//r-statistics/serzhby')
# выбрать временной ряд из БЖД. построить модель множественной регрессии.
# попытаться смоделировать сезонность. 
data = read.csv('bzd.csv', header = TRUE, sep = ';')
s = c('1', '2', '3', '4')
seasons = c()
trend = c()
for(a in seq(1, 7)) {
  seasons = c(seasons, s)
}
for(a in 1:17) {
  trend = c(trend, 0)
}
for(a in 1:11) {
  trend = c(trend, 1)
}

data$trend = trend
data$seasons = seasons

data$ts = ts(data$MINSK, frequency = 4)
plot(data$ts)

t = 1:28
plot(t, data$ts, type="l")
dataReg = lm(data$ts ~ t + data$seasons + data$trend)
lines(t, fitted(dataReg), col="red")
summary(dataReg)
# проверка автокорреляции в остатках
res = residuals(dataReg)
acf(res, lag.max = 20) #ACF  автокорреляционная функция
for (i in 1:15) {
  bt = Box.test(res, lag = i, type = "Ljung-Box")
  print(c(bt$statistic, bt$p.value))
}
# есть автокорреляция в остатках

pearson.test(res)
# остатки распределены нормально

bptest(dataReg)

hw = HoltWinters(data$ts, beta=FALSE, gamma=FALSE)
accuracy(dataReg)

