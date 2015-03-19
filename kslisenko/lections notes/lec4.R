## тест Дарвина-Уотсона
## если автокорреляция приближается к -1, DW приближается к 4, если к 1, то DW приближается к 0
## если автокорреляция близится к 0, то DW близится к 2
## далее анализируем коэффициент и делаем выводы: отрицательная корреляция, положительная, нет, или неопределённость
## подходит только для проверки автокорреляции 1-го порядка (зависит ли текущее значение от предыдущего) - но не от значения 3 точки назад

## тест Бреуша-Годфри - проверяет автокорреляцию любого порядка (порядок подставляется в формулу)

install.packages("scatterplot3d")

library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)

# построение множественной регрессии
s3s = scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, pch=16, type="h")
m2 = lm(mpg ~ wt + hp, data = mtcars)
s3s$plane3d(m2)
summary(m2)

## анализ модели
## график остатков
residualPlot(m2)
plot(residuals(m2), type="b")
## тест Дарвина-Уотсона
durbinWatsonTest(m2)
## тест Бреуша-Годфри - автокорреляция 1-го порядка
bgtest(m2, order=1, type="F")
bgtest(m2, order=1, type="Chisq")
## 2-го порядка
bgtest(m2, order=2, type="F")
bgtest(m2, order=2, type="Chisq")
## 3-го порядка
bgtest(m2, order=3, type="F")
bgtest(m2, order=3, type="Chisq")

## гетероскедастичность в остатках отсутствует
plot(fitted(m2), residuals(m2)) ## зависимость остатков от предсказанных регрессией значений
bptest(m2)
ncvTest(m2) ## p-value=0.4 => отсутствует гетероскедастичность
spreadLevelPlot(m2) ## стандартизированные остатки от предсказанных регрессией значений 

## проверка на нормальность распределения
e = residuals(m2)
hist(e, breaks=15, col="lightblue", freq=FALSE)
## моделируем нормальное распределение, и рисуем его график поверх гистограммы
xfit = seq(min(e), max(e), length=40)
yfit = dnorm(xfit, mean=mean(e), sd=sd(e))
lines(xfit, yfit, col="blue", lwd=2)

## нормальное распределение на вероятностной бумаге
qqnorm(residuals(m2))
qqline(residuals(m2))
## тест Пирсона на нормальное распределение
pearson.test(residuals(m2))
spreadLevelPlot(m2)

## другие аспекты анализа
outlierTest(m2) ## обнаружение выбросов - проверка наибольшего остатка регрессии на разницу с центром выборки
## Toyota Corolla - сильноотличающееся распределение - может быть выбросом (если Bonferroni p < 0.05)
## фактор инфляции дисперсии (мультиколлинеарность)
vif(m2)
plot(m2)
crPlots(m2)

## квартиры - зависимость цены от всех площадей
flats <- read.csv("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/flats.csv", sep=";")
## зависимость цены от площадей - на больших площадях большой разброс
plot(flats$Cena~flats$PlOb)
## попробуем с логарифмами - меньше разброс, видна линейная зависимость
flats$Cena_log = log(flats$Cena)
flats$PlOb_log = log(flats$PlOb)
plot(flats$Cena_log~flats$PlOb_log)

## строим регрессию
flatReg = lm(flats$Cena_log ~ flats$PlOb_log + flats$ketaz + flats$Type)
summary(flatReg)
## проверяем выполнимость модельных предположений
## тест Дарвина-Уотсона
durbinWatsonTest(flatReg)
## тест Бреуша-Годфри - автокорреляция 2-го порядка
bgtest(flatReg, order=2, type="F")
bgtest(flatReg, order=2, type="Chisq")
## тест Бреуша-Годфри - автокорреляция 3-го порядка
bgtest(flatReg, order=3, type="F")
bgtest(flatReg, order=3, type="Chisq")

plot(fitted(flatReg), residuals(flatReg)) ## зависимость остатков от предсказанных регрессией значений
bptest(flatReg)
ncvTest(flatReg) ## p-value=6.001305e-12 => присутствует гетероскедастичность - данные неоднородные
spreadLevelPlot(flatReg) ## стандартизированные остатки от предсказанных регрессией значений 
## строим boxplot для каждого района - посмотреть выбросы
flats$Rayon = flats$Район
boxplot(flats$Cena ~ flats$Rayon)

## берём Партизанский район
partizansky = subset(flats, Rayon == "Партизанский")
partizanskyReg = lm(partizansky$Cena_log ~ partizansky$PlOb_log + partizansky$ketaz + partizansky$Type)
summary(partizanskyReg)
## тест Дарвина-Уотсона
durbinWatsonTest(partizanskyReg)
bgtest(partizanskyReg, order=2, type="F")
bgtest(partizanskyReg, order=2, type="Chisq")
bptest(partizanskyReg)
ncvTest(partizanskyReg) ## гетероскедастичность отсутствует - значит можно использовать для анализа - хорошая модель
plot(residuals(partizanskyReg)) ## график остатков
scatterplot(partizansky$Cena_log, partizansky$PlOb_log + partizansky$ketaz + partizansky$Type)