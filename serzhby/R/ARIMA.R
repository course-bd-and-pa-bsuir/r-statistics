library(astsa)
library(tseries)
library(nortest)
help(astsa)
data(jj) #Johnson & Johnson data set
help(jj)
time(jj)     
cycle(jj)    
frequency(jj)
plot(jj, ylab="Earnings per Share", main="J & J") 
plot(jj, type="o", col="blue", lty="dashed")
plot(diff(log(jj)), main="logged and diffed") # возможное приведение ряда к стац. виду 
acf(jj) # автокорреляционная функция
dljj = diff(log(jj))
# расширенный тест дики фюллера. h0 - ряд не стационарный
adf.test(jj)
# оригинальный ряд - нестационарный
adf.test(diff(jj))
# дифференцированный - стационарный
pp.test(jj)
pp.test(diff(jj))
kpss.test(jj, null = "Trend")
kpss.test(diff(jj), null = "Trend")
kpss.test(jj, null = "Level")
kpss.test(diff(jj), null = "Level")
# по результатам 3х тестов - оригинальный ряд не стационарный, а diff стационарный

# модель авторегрессии
# p - уровень авторегрессии
# d - степень дифференцирования
# q - степень MA (скользящего среднего)
ar = arima(jj, order = c(1, 1, 1))
res = residuals(ar)
acf(res)
stljj = stl(jj, s.window="periodic")
plot(stljj)
for (i in 1:6) {
  bt = Box.test(res, lag = i, type = "Ljung-Box")
  print(bt$p.value)
}

#?????????? - ?????????? ???????
k = c(.5,1,1,1,.5)    
(k = k/sum(k))   
fjj = filter(jj, sides=2, k)
plot(jj)
lines(fjj, col="red")  
lines(lowess(jj), col="blue", lty="dashed")
plot(dljj)   
shapiro.test(dljj)        
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)     
lag.plot(dljj, 9, do.lines=FALSE)  
lag1.plot(dljj, 9)
acf(dljj, 20) 
plot(dog <- stl(log(jj), "per"))   
Q = factor(cycle(jj)) 
trend = time(jj)-1970 
trend
reg = lm(log(jj)~0+trend+Q, na.action=NULL)
summary(reg)
model.matrix(reg)
plot(log(jj), type="o")   # the data in black with little dots 
lines(fitted(reg), col=2)
ar1=arima(jj,order=c(1,0,0))
ar1=arima(jj,order=c(1,0,0),xreg=trend)
ar1
acf(residuals(ar1))
coef(ar1)
tsdiag(ar1.fit,gof.lag=20)
#ts=lag(b.ts,-1) ??? ?????????? ????
#http://www.stat.pitt.edu/stoffer/tsa3/R_toot.htm
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.html