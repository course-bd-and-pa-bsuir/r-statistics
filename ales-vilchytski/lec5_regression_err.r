# Лекция 5 - ошибки регрессии

library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)

# Построение множественное регрессии
s3d <- scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, pch=16, type="h", highlight.3d=TRUE, col.grid="blue")
m2 <- lm(mpg~wt+hp, data=mtcars)
s3d$plane3d(m2)

# R-sq хороший
# F при p-value хороший
# Вывод: модель хорошая
summary(m2)

# Вычисление теоретических y
fitted(m2)

# Значения коэффициентов и доверительные интервалы:
coef(m2)
confint(m2)

# Ошибки регрессии, автокорреляция:

# График остатков
residualPlot(m2)
plot(residuals(m2), type="b")

# Анализ модели на автокоррел¤цию
# ! выдаёт разные p-value при разных запусках. Это не баг, это фича!
durbinWatsonTest(m2)

bgtest(m2, order=1, type="F") # По F-статистике
bgtest(m2, order=1, type="Chisq") # По х^2

bgtest(m2, order=2, type="Chisq")
# Вывод: автокорреляция в остатках отсутствует, так как p-value > 0.05
# ============


# Гетероскедастичность
plot(fitted(m2), residuals(m2))
bptest(m2) # p > 0.05 - Тест показывает отсутствие гетероскедастичности

ncvTest(m2) # Аналогично
spreadLevelPlot(m2)

# Проверка распределени случайных ошибок регрессии
e <- residuals(m2)
hist(e,breaks=50,col="lightblue",freq=FALSE)
xfit<-seq(min(e), max(e), length=40)
yfit<-dnorm(xfit, mean=mean(e), sd=sd(e))
lines(xfit, yfit, col="blue", lwd=2)

qqnorm(residuals(m2))
qqline(residuals(m2))

# Тесты на нормальность
pearson.test(residuals(m2)) # Остатки имеют нормальное распределение с p=0.84
spreadLevelPlot(m2) 
# ============


# Другие аспекты

# Обнаружение выбросов
# Выдаёт крайний отличающийся остаток
# Если Bonferonni p > 0.05, то это НЕ выброс
outlierTest(m2) 

# Фактор инфляции дисперсии (мультиколлинеарность)
# Есть две оценки - либо корень значения коэфф. долежн быть < 2, либо значение д.б. < 10
vif(m2)

# Зависимость остатков и компонент от факторов
# Можно увидеть разброс
crPlots(m2)
# ===========
