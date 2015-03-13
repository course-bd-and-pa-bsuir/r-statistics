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
library(moments)

data <- read.csv('data/flats_plus_geodata.csv', header=TRUE)
 
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
summary(fitK)

fitD <- lm(LnCena~(LnPlZ+LnPlK+Type+Nkomnat+LnCenterDistance), data=lp)
anova(fitK, fitD)
AIC(fitK, fitD)
summary(fitD) # Type незначим

fitD1 <- lm(LnCena~(LnPlZ+LnPlK+Nkomnat+LnCenterDistance), data=lp)
summary(fitD1)

fitD2 <- lm(LnCena~(LnPlZ+LnPlK+Nkomnat+LnCenterDistance+LnStationDistance), data=lp)
anova(fitD, fitD2)
AIC(fitD, fitD2)

# Проверим качество при введении признака крайнего этажа
fitE <- lm(LnCena~(LnPlZ+LnPlK+Nkomnat+LnCenterDistance+LnStationDistance+ketaz), data=lp)
# p > 0.05, введение параметра не улучшило модель
anova(fitD2, fitE)
AIC(fitD2,fitE)

# Все параметры статистически значимы, R-sq модели 0.8562, что свидетельствует о большой объяснённой части выборки
fit <- fitD2
summary(fit)


# Проверка корректности модели
check.fit <- function(fit, p.value = 0.05) {
  res <- c()
  
  # Если p > 0.05 - распределение нормальное
  res$NORM_JQ = jarque.test(residuals(fit))$p.value
  res$NORM_PE = pearson.test(residuals(fit))$p.value
  res$NORM_OK = res$NORM_JQ > p.value && res$NORM_PE > p.value
  
  # Автокорреляция отсутствует при p > 0.05
  res$AC_DW = durbinWatsonTest(fit)$p
  res$AC_BGF1 = bgtest(fit, order=1, type="F")$p.value
  res$AC_BGF2 = bgtest(fit, order=2, type="F")$p.value
  res$AC_BGC1 = bgtest(fit, order=1, type="Chisq")$p.value
  res$AC_BGC2 = bgtest(fit, order=2, type="Chisq")$p.value
  res$AC_OK = res$AC_DW > p.value || 
    (res$AC_BGF1 > p.value && 
       res$AC_BGF2 > p.value && 
       res$AC_BGC1 > p.value && 
       res$AC_BGC2 > p.value)
  
  # Модель гомоскедастична при p > 0.05
  res$GS_BP = bptest(fit)$p.value
  res$GS_NCV = ncvTest(fit)$p
  res$GS_GQ = gqtest(fit)$p.value
  res$GS_OK = res$GS_BP > p.value && res$GS_NCV > p.value && res$GS_GQ > p.value
  
  res$ALL_OK = res$NORM_OK && res$AC_OK && res$GS_OK
  
  # Визуализация:
  #  - график остатков
  mfrow <- par("mfrow")
  par(mfrow=c(1,3))
  plot(residuals(fit), type="b")
  
  # - гистограмма с линией нормального распределения (видны выбросы)
  ress <- residuals(fit)
  h <- hist(ress, breaks=20) # гистограмма остатков
  xfit<-seq(min(ress),max(ress),length=40) 
  yfit<-dnorm(xfit,mean=mean(ress),sd=sd(ress)) 
  yfit <- yfit*diff(h$mids[1:2])*length(ress) 
  lines(xfit, yfit, col="black", lwd=2)
  
  # - график распределения теоретического и реального норм. распределения
  qqnorm(residuals(fit))
  qqline(residuals(fit))
  par(mfrow=mfrow)
  
  return(res)
}

check.fit(fit)
# Распределение остатков нормальное
# jarque.test -> 0.365
# pearson.test -> 0.502
# Автокорреляция в норме:
# DW -> 0.032
# bgtest -> 0.06-0.27
# Гетероскедастичность в норме:
# bptest -> 0.003
# ncvTest -> 0.0022
# gqtest -> 0.5
#
# Вывод: модель на грани фола. Часть критериев показывает наличие автокорреляции 
# и гетероскедастичности, под вопросом нормальность распределения остатков
# На гистограмме с линией нормального распределения видны выбросы


fit1 <- lm(LnCena~(LnPlZ+LnPlK+Nkomnat+LnCenterDistance+LnStationDistance), data=data[data$Rayon=='Фрунзенский',])
check.fit(fit1)

fit2 <- lm(LnCena~(Rayon+LnPlZ+LnPlK+Nkomnat), data=data)
summary(fit2)

check.fit(fit) # Распределение остатков отлично от нормального

# Введём фиктивные переменные со значениями для особых квартир, остатки по которым выходят за пределы +-0.5
ress <- residuals(fit)
indices = which(ress > 0.5 | ress < -0.5, arr.ind = TRUE)
data$wtf = 0
data[indices,]$wtf = 1

fitD1 <- lm(LnCena~(LnPlZ+LnPlK+Nkomnat+LnCenterDistance+LnStationDistance+wtf), data=data)
summary(fitD1)
anova(fit, fitD1)
check.fit(fitD1)

fitD2 <- lm(LnCena~(LnPlZ+LnPlK+I(LnStationDistance^2)+Type), data=data)
anova(fitD1, fitD2)
AIC(fitD1, fitD2)

summary(fitD2)
check.fit(fitD2) # Теперь распределение нормальное, но гетероскедастичность большая
par(mfrow=c(1,1))
plot(residuals(fit), type="b")



# Всё-таки будем делать разбивку по районам, просто добавим данных

model = LnCena~(LnPlZ+LnPlK+LnCenterDistance)
d = data[data$Rayon=='Партизанский',]
durbinWatsonTest(fit)
fit <- lm(model, data=d)
summary(fit)
check.fit(fit)
