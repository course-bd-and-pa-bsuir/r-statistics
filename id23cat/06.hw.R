library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(gplots)
library(MASS)
library(ggplot2)

data = read.table("kvart.csv", dec=",", sep=";", header=TRUE, fill=FALSE)
subP = subset(x=data, subset=data$District=="Партизанский")
subF = subset(x=data, subset=data$District=="Фрунзенский")
subO = subset(x=data, subset=data$District=="Октябрьский")
subZ = subset(x=data, subset=data$District=="Заводской")
subPe = subset(x=data, subset=data$District=="Первомайский")
subS = subset(x=data, subset=data$District=="Советский")
subC = subset(x=data, subset=data$District=="Центральный")
subL = subset(x=data, subset=data$District=="Ленинский")

m1 = lm(flats$LnCena ~ flats$LnPlOb)
ma = lm(flats$LnCena ~ flats$LnPlZ)
mb = lm(flats$LnCena ~ flats$LnPlK)
mab = lm(flats$LnCena ~ flats$LnPlZ + flats$LnPlK)

su1 = summary(m1)
sua = summary(ma)
sub = summary(mb)
suab = summary(mab)

# проверяем значимость добавления фактора
anova(ma, mab) # фактор улучшил модель

# сравнваем модели
AIC(m1,mab) #m1 лучше

m2 = lm(flats$LnCena ~ flats$LnPlOb+flats$Side)
summary(m2)

anova(m1, m2)
qqnorm(residuals(m2))
qqline(residuals(m2))
qqPlot(m2)

pearson.test(residuals(m2)) #p<0.05 модель плохая




mP2 = lm(subP$LnCena ~ subP$Nkomnat)
mP2 = lm(subP$LnCena ~ subP$Nkomnat+subP$ketaz)
summary(mP)

cor.test(subP$LnCena, subP$ketaz)

plotmeans(subP$LnCena ~ subP$ketaz, paired=TRUE)
boxplot(subP$LnCena, subC$LnCena, subS$LnCena, )

boxplot(flats$LnCena ~ flats$Side)
