# Анализ данных о квартирах Минска
# Задача: Построить модель данных для всех переменных, включая фиктивные ketaz и type, проверить качество модели

library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(moments)

data <- read.csv('data/lab5_flat.csv', header=TRUE)

# Сравним диаграмму рассеяния для логарифмических параметров и нелогарифмических
par(mfrow=c(1,2))
plot(data$Cena, data$Plob)
plot(data$LnCena, data$LnPlob)
# Логарифмические данные получше

# Первый подход к снаряду
fit <- lm(LnCena~(Nkomnat+LnPlZ+LnPlK+ketaz+Type), data=data)
# Коэффициенты (кроме этажности) значимы, R=0.8, p-value удовлетворительный
summary(fit)
# НО надо проверить автокорреляцию и гетероскедастичность

plot(residuals(fit), type="b")
durbinWatsonTest(fit)

# Автокорреляция присутствует (p-value < 0.05)
bgtest(fit, order=1, type="F") # По F-статистике
bgtest(fit, order=1, type="Chisq") # По х^2
bgtest(fit, order=2, type="Chisq") # второго порядка

# !!! Гетероскедастичность зашкаливает
bptest(fit)
# Вывод: модель нерелевантна
# ==========


# Второй подход к снаряду
# Если выкинуть все "лишние" данные, то получим неплохой с виду результат - цена зависит от площади и типа дома
fit <- lm(LnCena~(LnPlZ+LnPlK+Type), data=data)
summary(fit)
# НО надо проверить автокорреляцию и гетероскедастичность:

# График остатков показывает выбросы
plot(residuals(fit), type="b")
durbinWatsonTest(fit)

# Автокорреляция отсутствует (p-value > 0.05)
bgtest(fit, order=1, type="F") # По F-статистике
bgtest(fit, order=1, type="Chisq") # По х^2
bgtest(fit, order=2, type="Chisq") # второго порядка

# !!! Гетероскедастичность зашкаливает
bptest(fit)
# Вывод: модель нерелевантна
# ==========

# TODO: разбить данные по районам и построить модели, если ок - проверить тестом Пирсона и Жака-Бера

model = LnCena~(Nkomnat+LnPlZ+LnPlK+ketaz+Type)
p.val = 0.05

results = data.frame(Rayon = NULL, 
                     AutoCorr = NULL, 
                     GetSK = NULL,
                     JarqueNorm = NULL,
                     PearsonNomr = NULL)

par(mfrow=c(3, floor(nlevels(data$Rayon)/3)))
for (r in levels(data$Rayon)) {
  rdata = data[data$Rayon == r,]
  fit = lm(model, data=rdata)
  plot(residuals(fit), type="b", main=r)
  
  # Автокорреляция отсутствует при p > 0.05
  corDW = durbinWatsonTest(fit)$p
  cor1 = bgtest(fit, order=1, type="F")$p.value
  cor2 = bgtest(fit, order=2, type="F")$p.value
  
  # Модель гомоскедастична при p > 0.05
  sked1 = bptest(fit)$p.value
  sked2 = ncvTest(fit)$p.value
  
  # Если критерий более 0.05 - распределение нормальное
  jq = jarque.test(residuals(fit))$p.value
  ps = pearson.test(residuals(fit))$p.value
  
  results <- rbind(results, data.frame(Rayon = r, 
                                       AutoCorr = corDW < p.val || cor1 < p.val || cor2 < p.val, 
                                       GetSK = sked1 < p.val || sked2 < p.val,
                                       JarqueNorm = jq > p.val,
                                       PearsonNorm = ps > p.val))
  
}

# Т.о. получили для модели:
# - графики остатков по районам
# - таблицу с указанием наличия автокорреляции, гетероскедастичности, 
# а также нормальности распределения по критериям Жака-Бера и Пирсона
model
results
