setwd('~/code/R/BDPA/r-statistics/serzhby')
# выбрать временной ряд из БЖД. построить модель множественной регрессии.
# попытаться смоделировать сезонность. 

source('L7_prepare_data.R')
source('test_lm.R')

data = prepareData()

plot(data$ts, xlab = "Год", ylab = "Объём")
plot(decompose(data$ts), xlab = "Год")

t = 1:28
plot(t, data$ts, type="l", xlim = c(1, 40), ylim = c(500, 2000))
dataReg = lm(ts ~ X + seasons + trend, data = data)
dataReg = lm(ts ~ X + seasons, data = data)

testModel(dataReg, verbose = TRUE)

newdata = data.frame(X = c(28:35), seasons = c('4', '1', '2', '3'), trend = c(0))
prediction = predict(dataReg, newdata, interval = "predict")

prediction = as.data.frame(prediction)

lines(28:35, prediction$upr, col="blue")

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

