# Анализ квартир с учётом качества моделей
# 
# Результат сравнения качества модели LnCena~(Nkomnat+LnPlZ+LnPlK+ketaz+Type) по отдельным районам
#          Rayon NoAutoCorr NoGetSK JarqueNorm PearsonNorm
# 1    Заводской       TRUE    TRUE       TRUE        TRUE
# 2    Ленинский       TRUE   FALSE       TRUE        TRUE
# 3   Московский       TRUE    TRUE       TRUE        TRUE
# 4  Октябрьский       TRUE   FALSE       TRUE        TRUE
# 5 Партизанский       TRUE    TRUE       TRUE        TRUE
# 6 Первомайский       TRUE   FALSE      FALSE       FALSE
# 7    Советский       TRUE    TRUE      FALSE        TRUE
# 8  Фрунзенский       TRUE   FALSE      FALSE       FALSE
# 9  Центральный       TRUE    TRUE       TRUE        TRUE

library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(gplots)
library(MASS)
library(ggplot2)

data <- read.csv('data/lab5_flat.csv', header=TRUE)

zav = data[data$Rayon == 'Заводской',]
len = data[data$Rayon == 'Ленинский',]
mos = data[data$Rayon == 'Московский',]
oct = data[data$Rayon == 'Октябрьский',]
par = data[data$Rayon == 'Партизанский',]
per = data[data$Rayon == 'Первомайский',]
sov = data[data$Rayon == 'Советский',]
fru = data[data$Rayon == 'Фрунзенский',]
cen = data[data$Rayon == 'Центральный',]

# Сравнение средних значений выборок "каждый с каждым"
results = data.frame(Rayon1 = NULL, 
                     Rayon2 = NULL, 
                     WilcoxP = NULL)

for (r in levels(data$Rayon)) {
  for (r1 in levels(data$Rayon)) {
    results <- rbind(results, 
                     data.frame(
                       Rayon1 = r, 
                       Rayon2 = r1, 
                       WilcoxP = wilcox.test(
                         data[data$Rayon == r,]$LnCena, 
                         data[data$Rayon == r1,]$LnCena
                       )$p.value))
  }
}

chained <- results[results$WilcoxP > 0.8 & results$WilcoxP < 1,]
# По результату возьмём Ленинский и Первомайский районы, как наиболее близкие друг к другу
lp  <- rbind(len, per)

# Начнём с модели с малым количеством параметров
fit <- lm(LnCena~(LnPlZ+LnPlK+Type), data=lp)
summary(fit)

# Проверим качество при введении кол-ва комнат в модель:
fitK <- lm(LnCena~(LnPlZ+LnPlK+Type+Nkomnat), data=lp)
# p < 0.05, введение параметра улучшило модель
anova(fit, fitK)
AIC(fit, fitK)

# Проверим качество при введении признака крайнего этажа
fitE <- lm(LnCena~(LnPlZ+LnPlK+Type+Nkomnat+ketaz), data=lp)
# p > 0.05, введение параметра не улучшило модель
anova(fitK, fitE)
AIC(fitK,fitE)

# Все параметры статистически значимы, R-sq модели 0.79, что свидетельствует о большой объяснённой части выборки
fit <- fitK
summary(fit)

# Проверка корректности модели
# Автокорреляция отсутствует при p > 0.05
durbinWatsonTest(fit) # 0.038
bgtest(fit, order=1, type="F") # 0.11
bgtest(fit, order=2, type="F") # 0.29
bgtest(fit, order=1, type="Chisq") # 0.11
bgtest(fit, order=2, type="Chisq") # 0.27

# Модель гомоскедастична при p > 0.05
bptest(fit) # 0.07
ncvTest(fit) # 0.022

# Если p > 0.05 - распределение нормальное
jarque.test(residuals(fit)) # 0.0005
pearson.test(residuals(fit)) # 0.157

# Вывод: модель на грани фола. Часть критериев показывает наличие автокорреляции 
# и гетероскедастичности, под вопросом нормальность распределения остатков
# Визуализация:
#  - график остатков
par(mfrow=c(1,3))
plot(residuals(fit), type="b")

# - гистограмма с линией нормального распределения (видны выбросы)
ress <- residuals(fit)
h <- hist(ress, breaks=20) # гистограмма остатков
xfit<-seq(min(ress),max(ress),length=40) 
yfit<-dnorm(xfit,mean=mean(ress),sd=sd(ress)) 
yfit <- yfit*diff(h$mids[1:2])*length(ress) 
lines(xfit, yfit, col="black", lwd=2)

#  - график распределения теоретического и реального норм. распределения
qqnorm(residuals(fit))
qqline(residuals(fit))
