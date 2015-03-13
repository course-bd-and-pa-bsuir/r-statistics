#подключение библиотек
library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(gplots)
library(MASS)
library(ggplot2)
#построение множественной регрессии 
s3d <-scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg,pch=16,type="h",highlight.3d=TRUE,col.grid="blue")
m2=lm(mpg~wt+hp,data=mtcars)
m3=lm(mpg~wt+hp+disp,data=mtcars)
s3d$plane3d(m2)
#вывод результатов 
summary(m2)
#вычисление теоретических y
fitted(m2)
#значения коэффициентов и их доверительные интервалы
coef(m2)
confint(m2)
#Анализ модели
#на автокорреляцию
bgtest(m2,order=1,type="F")
durbinWatsonTest(m2)
# график остатков модели
residualPlot(m2)
plot(residuals(m2),type="b")
#проверка на гетероскедастичность остатков
plot(fitted(m2),residuals(m2))
bptest(m2)
ncvTest(fit)
spreadLevelPlot(m2)
#проверка распределения случайных ошибок регрессии
e=residuals(m2)
hist(e,breaks=50,col="lightblue",freq=FALSE)
xfit<-seq(min(e), max(e), length=40)
yfit<-dnorm(xfit, mean=mean(e), sd=sd(e))
lines(xfit, yfit, col="blue", lwd=2)
qqnorm(residuals(m2))
qqline(residuals(m2))
#тесты на нормальность
pearson.test(residuals(m2))
spreadLevelPlot(m2)
#другие аспекты анализа
#обнаружение выбросов
outlierTest(m2)
#фактор инфляции дисперсии (мультиколлинеарность)
vif(m2)
##
plot(m2)
crPlots(m2)
# сравнение моделей
anova(m2,m3) #тест на значимость новых переменных
anova(m2)
#AIC
AIC(m2,m3)
#Тесты Стьюдента (одновыборочный)
t.test(mtcars$mpg,mu=20)
#тест Уилкоксона (одновыборочный)
wilcox.test(mtcars$wt,mu=4)
#выборочные сравнения
t.test(mtcars$mpg~mtcars$vs) #тест Стьюдента для независимых 
wilcox.test(mtcars$mpg~mtcars$vs)
#выборок,по eмолчанию дисперсии не равны, иначе ----var.equal = TRUE
t.test(mtcars$cyl,mtcars$am,paired=TRUE)#-#-для зависимых выборок
# ДА
stripchart(mtcars$mpg~mtcars$cyl)
boxplot(mtcars$mpg~mtcars$cyl)
plotmeans(mtcars$mpg~mtcars$cyl)

#непараметрические тесты множественных сравнений
kruskal.test(mtcars$mpg~mtcars$cyl)
??npmc
install.packages("npmc")
library(npmc)
install.packages("forecast")
library(forecast)
library(zoo)
Birth
bd=Birth
       