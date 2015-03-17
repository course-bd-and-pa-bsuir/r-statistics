library(astsa)
library(tseries)
library(nortest)
library(TTR)
library(car)sdfsdfsdf




library(lmtest)
library(nortest)
library(forecast)
help(astsa)
data(jj) #Johnson & Johnson data set
help(jj)
time(jj)     
cycle(jj)    
frequency(jj)
plot(jj, ylab="Earnings per Share", main="J & J") 
plot(jj, type="o", col="blue", lty="dashed")
## логарифмирование уменьшает разброс
## дифференцирование выполняет последовательную разность
## делаем ряд более похожим на стационарный
diffLogJJ = diff(log(jj))
plot(diffLogJJ, main="logged and diffed") 
## проверяем стационарность
adf.test(jj) ## сначала просто ряд, нулевая гипотеза "ряд-нестационарный", по тесту он нестационарный
## диффиренцирование - переход к приростному показателю, вычитание у каждого элемента значения прошлого элемента, теряется первый элемент
adf.test(diffLogJJ) ## дифференцированный ряд - стационарный
pp.test(jj)
pp.test(diffLogJJ)
kpss.test(jj)
kpss.test(diffLogJJ) ## этот тест говорит что не стационарный, но 2 теста уже сказали что да
## diff(jj) - стационарный, можем дальше брать этот ряд и строить по нему модели
## в практике используются 3 теста, результат берётся по двум совпавшим тестам
## по результатам трёх тестов ряд - нестационарный, а diff - стационарный
acf(jj, 35) ## автокорелляционная функция => сильная корреляция
acf(diffLogJJ, 35) ## автокорелляционная функция => сильная корреляция
#?????????? - ?????????? ???????

## делаем авторегрессию с мопощью arima
diffFfReg = arima(diffLogJJ, order=c(1,0,0)) ## 1,0,0- авторегрессия первого порядка, 2,0,0 - второго порядка
## второй аргумент - нужно ли дифференцировать входной ряд
## третий аргумент - скользящее среднее, влияние случайного воздействия прошлых периодов
plot(residuals(diffFfReg))
## оценка автокорреляции
acf(residuals(diffFfReg), lag.max = 10)

for (i in 1:15) {
  boxJung = Box.test(residuals(diffFfReg), lag=i, type="Ljung-Box")
  print(c("lag", i, "p-value", boxJung$p.value))
}

## уточним сезонность
diffFfReg = arima(diffLogJJ, order=c(1,0,0), seasonal = list(order=c(1,0,0)))
diffFfReg = arima(diffLogJJ, order=c(1,0,0), seasonal = list(order=c(1,0,1)))
plot(residuals(diffFfReg))
acf(residuals(diffFfReg), lag.max = 10)
for (i in 1:15) {
  boxJung = Box.test(residuals(diffFfReg), lag=i, type="Ljung-Box")
  print(c("lag", i, "p-value", boxJung$p.value))
}

pearson.test(residuals(diffFfReg)) ## нормальное распределение
jarque.bera.test(residuals(diffFfReg)) ## нормальное распределение


pearson.test(residuals(diffFfReg)) ##


k = c(.5,1,1,1,.5)    
(k = k/sum(k))   
## фильтр = сглаживание данных
fjj = filter(jj, sides=2, k) 
plot(jj)
lines(fjj, col="red")  
lines(lowess(jj), col="blue", lty="dashed")
dljj = diff(log(jj))
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


bgd <- read.csv("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/bgd.csv", sep=";")
bgdReg = lm(bgd$MINSK ~ bgd$X + bgd$season1 + bgd$season2 + bgd$season3 + bgd$trend) ## + lag(bgd$MINSK, -4)
summary(bgdReg)
pearson.test(residuals(bgdReg)) ## нормальное распределение
jarque.bera.test(residuals(bgdReg))
bptest(bgdReg) ## гетероскедастичность присутствует
bgtest(bgdReg) ## есть автокорреляция
plot(residuals(bgdReg))
plot(bgd$MINSK, type="b")
