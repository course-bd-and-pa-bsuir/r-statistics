setwd('~/code/R//BDPA/r-statistics/serzhby/')
flats = read.csv('flats.csv', sep = ';')

colnames(flats) = c('id', 'address', 'district', 'cost', 'lnCost', 'costM', 'roomCount', 'elit', 'areaAll', 'lnAreaAll', 'areaZ', 'lnAreaZ', 'areaKitchen', 'lnAreaKitchen', 'ketazh', 'type', 'year', 'phone', 'balcon')

attach(flats)
# диаграмма цены от площадей
plot(cost ~ areaAll)

plot(lnCost ~ lnAreaAll)
# вывод: разброс лог меньше

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
# after bg tests - ok

# построить модель регрессии для всех данных. включить ketazh и type. проверить качество модели.
bptest(linModel)
ncvTest(linModel)
# very bad results.
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
# no autocorrelation

bptest(subLinModel)

plot(residuals(subLinModel), type = 'b')
library(moments)
jarque.test(residuals(subLinModel))
# hist(residuals(subLinModel))
