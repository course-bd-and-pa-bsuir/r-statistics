setwd('~/code/R//r-statistics/serzhby/')

library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(tseries)
library(moments)

flats = read.csv('flats.csv', sep = ';')
flats = subset(flats, district != "")

colnames(flats) = c('id', 'address', 'district', 'cost', 'lnCost', 'costM', 'roomCount', 'elit', 'areaAll', 'lnAreaAll', 'areaZ', 'lnAreaZ', 'areaKitchen', 'lnAreaKitchen', 'ketazh', 'type', 'year', 'phone', 'balcon')

attach(flats)
# диаграмма цены от площадей
plot(cost ~ areaAll)

plot(lnCost ~ lnAreaAll)
# вывод: разброс лог меньше
# plot(lnCost ~ lnAreaKitchen + lnAreaZ)

linModel = lm(lnCost ~ lnAreaKitchen + lnAreaZ)
summary(linModel)

linModel = lm(lnCost ~ lnAreaKitchen + lnAreaZ + ketazh + type)
summary(linModel)
# all parameters are valueable

plot(residuals(linModel), type = 'b')

bgtest(linModel, order = 1, type = 'F')
bgtest(linModel, order = 1, type = 'Chisq')

bgtest(linModel, order = 2, type = 'F')
bgtest(linModel, order = 2, type = 'Chisq')

bgtest(linModel, order = 2, type = 'F')
bgtest(linModel, order = 2, type = 'Chisq')
# after bg tests - ok. p-value > 5% - no autocorrelation

# построить модель регрессии для всех данных. включить ketazh и type. проверить качество модели.
bptest(linModel)
ncvTest(linModel)
# very bad results. высокая гетероскедастичность. p-value < 5%
# now try to find by districts

boxplot(cost ~ district)
# now select district and try to do the same in one
subFlats = subset(flats, district == "Первомайский")
subLinModel = lm(lnCost ~ lnAreaKitchen + lnAreaZ + ketazh + type, data = subFlats)
summary(subLinModel)
# ketazh and type are not valueable

subLinModel = lm(lnCost ~ lnAreaKitchen + lnAreaZ, data = subFlats)
summary(subLinModel)

bgtest(subLinModel, order = 1, type = 'F')
bgtest(subLinModel, order = 1, type = 'Chisq')

bgtest(subLinModel, order = 2, type = 'F')
bgtest(subLinModel, order = 2, type = 'Chisq')

bgtest(subLinModel, order = 3, type = 'F')
bgtest(subLinModel, order = 3, type = 'Chisq')
# no autocorrelation. p-value > 5%

bptest(subLinModel)
# bptest seems to be ok

plot(residuals(subLinModel), type = 'b')
library(moments)
jarque.test(residuals(subLinModel))
# остатки распределены не нормально. модель плохая
# hist(residuals(subLinModel))




kruskal.test(flats$cost ~ flats$district)
kruskal.test(flats$lnCost ~ flats$district)
# районы не однородны по цене. надо выбрать 2 района, в которых одинаковы.

doubleSubset = subset(flats, district == c("Октябрьский", "Московский"), na = na.fail)
doubleSubset$dummy = 0
doubleSubset$dummy[6] = 1
doubleSubset$dummy[28] = 1

kruskal.test(doubleSubset$cost ~ doubleSubset$district)
wilcox.test(doubleSubset$cost ~ doubleSubset$district)
# для этих районов средние примерно равны. p-value > 5%

#octSubset = subset(flats, district == c("Октябрьский"))
#mosSubset = subset(flats, district == c("Московский"))
#sovSubset = subset(flats, district == c("Советский"))
#wilcox.test(octSubset$cost, mosSubset$cost)
#wilcox.test(sovSubset$cost, mosSubset$cost)
#wilcox.test(sovSubset$cost, octSubset$cost)

districtsEnum = levels(factor(flats$district))[-1]

doubleLinModel = lm(lnCost ~ lnAreaKitchen + lnAreaZ, data = doubleSubset)
summary(doubleLinModel)

linM1 = lm(lnCost ~ lnAreaKitchen + lnAreaZ + dummy, data = doubleSubset)
linM1 = doubleLinModel
linM2 = lm(lnCost ~ lnAreaKitchen + lnAreaZ + ketazh, data = doubleSubset)
#linM2 = lm(lnCost ~ lnAreaKitchen + lnAreaZ + year, data = doubleSubset)
# ввели новый фактор ketazh
anova(linM1, linM2)
# p-value > 5% - значит ketazh не влияет на регрессию
linM3 = lm(lnCost ~ lnAreaKitchen + lnAreaZ + type, data = doubleSubset)
anova(linM1, linM3)
# p-value = 0.069 - небольшое влияние type, но незначительное

residualPlot(linM1)

# проверим модель на качество.
durbinWatsonTest(linM1)
# p-value > 5%. no autocorrelation ??

for (i in 1:6) {
  bt = bgtest(linM1, order = i, type = 'F')
  print(c("bg statistic: ", bt$statistic, " p-value", bt$p.value))
}

bgtest(linM1, order = 1, type = 'F')
bgtest(linM1, order = 2, type = 'F')
bgtest(linM1, order = 3, type = 'F')
# p-value > 5% - no autocorrelation
res = residuals(linM1)
# for (i in 1:6) {
#   bt = Box.test(res, lag = i, type = "Ljung-Box")
#   print(bt$p.value)
# }

# проверяем на гетероскедастичность
bptest(linM1)
# p-value > 5%. нет гетероскедастичности

# проверим остатки на нормальность
pearson.test(res)
jarque.test(res)
# p-value must be > 5%. it's not, 4.5%
qqnorm(res)
qqline(res)
outlierTest(linM1)
