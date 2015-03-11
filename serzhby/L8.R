library(astsa)
library(tseries)
library(nortest)
dljj = diff(log(jj))
djj = diff(jj)

# тесты на стационарность
adf.test(jj)
adf.test(diff(jj))
pp.test(jj)
pp.test(diff(jj))
kpss.test(jj, null = "Trend")
kpss.test(diff(jj), null = "Trend")
kpss.test(jj, null = "Level")
kpss.test(diff(jj), null = "Level")

# модель авторегрессии
# p - уровень авторегрессии
# d - степень дифференцирования
# q - степень MA (скользящего среднего)
ar = arima(djj, order = c(1, 1, 1))
arseason = arima(dljj, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 1)))

ar = arseason

res = residuals(ar)
acf(res)
stljj = stl(jj, s.window="periodic")
plot(stljj)
for (i in 1:16) {
  bt = Box.test(res, lag = i, type = "Ljung-Box")
  print(bt$p.value)
}

pearson.test(res)
jarque.bera.test(res)

seasons = c()
for(a in 1:length(jj)) {
  seasons = c(seasons, a %% 4)
}

plot(dljj)
lines(predict(ar, n.ahead=48), col=2)
