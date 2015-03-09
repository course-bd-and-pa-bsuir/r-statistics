# Квартиры

flats = read.csv("05_flats_geo.csv", sep=";", dec=",", head=TRUE)

# Дискретные переменные по району - не использовал
flats$DistrictFru[flats$District == "Фрунзенский"] = 1
flats$DistrictFru[flats$District != "Фрунзенский"] = 0
flats$DistrictMos[flats$District == "Московский"] = 1
flats$DistrictMos[flats$District != "Московский"] = 0
flats$DistrictZav[flats$District == "Заводской"] = 1
flats$DistrictZav[flats$District != "Заводской"] = 0
flats$DistrictPrv[flats$District == "Первомайский"] = 1
flats$DistrictPrv[flats$District != "Первомайский"] = 0
flats$DistrictPrt[flats$District == "Партизанский"] = 1
flats$DistrictPrt[flats$District != "Партизанский"] = 0
flats$DistrictSov[flats$District == "Советский"] = 1
flats$DistrictSov[flats$District != "Советский"] = 0
flats$DistrictCen[flats$District == "Центральный"] = 1
flats$DistrictCen[flats$District != "Центральный"] = 0
flats$DistrictOct[flats$District == "Октябрьский"] = 1
flats$DistrictOct[flats$District != "Октябрьский"] = 0
flats$DistrictLen[flats$District == "Ленинский"] = 1
flats$DistrictLen[flats$District != "Ленинский"] = 0

# Ограничение по расстоянию до центра - бесполезно
flats$CenterDistanceNorm = flats$CenterDistance
flats$CenterDistanceNorm[flats$CenterDistance >= 5000] = 5000

# Ограничение по расстоянию до метро - бесполезно
flats$StationDistanceNorm = flats$StationDistance
flats$StationDistanceNorm[flats$StationDistance >= 5000] = 5000

# Дискретные переменные по году постройки - бесполезно
flats$StationNear[flats$StationDistanceNorm < 2500] = 1
flats$StationNear[flats$StationDistanceNorm >= 2500] = 0

# Дискретные переменные по году постройки - бесполезно
flats$Novostroyka[flats$God >= 2008] = 1
flats$Novostroyka[flats$God < 2008] = 0

# Дискретные переменные по количеству комнат - бесполезно
flats$NkOne[flats$Nkomnat == 1] = 1
flats$NkOne[flats$Nkomnat != 1] = 0
flats$NkTwo[flats$Nkomnat == 2] = 1
flats$NkTwo[flats$Nkomnat != 2] = 0
flats$NkThree[flats$Nkomnat >= 3] = 1
flats$NkThree[flats$Nkomnat < 3] = 0

# Отображает boxplot по цене квартир с группировкой по районам
boxplot(flats$Cena~flats$District, cex.axis=0.47)

# Выборки по районам и без выбросов
flats_m2 <- flats[which((flats$District == "Советский"|flats$District == "Партизанский")&flats$Id != 280&flats$Id != 209&flats$Id != 253&flats$Id != 202),]
flats_m2 <- flats[which((flats$District == "Центральный")&flats$Id != 274&flats$Id != 367&flats$Id != 324&flats$Id != 141),])
flats_m2 <- flats[which((flats$District == "Заводской")&flats$Id != 111),] # PlK = 2!
flats_m2 <- flats[which((flats$District == "Московский")&flats$Id != 327&flats$Id != 309&flats$Id != 80),]
flats_m2 <- flats[which((flats$District == "Фрунзенский")&flats$Id != 366&flats$Id != 342&flats$Id != 242),]
flats_m2 <- flats[which((flats$District == "Октябрьский")&flats$Id != 372&flats$Id != 362),]
flats_m2 <- flats[which((flats$District == "Первомайский")&flats$Id != 255&flats$Id != 43&flats$Id != 321&flats$Id != 252&flats$Id != 371),]

# с Ленинским ничего не вышло
flats_m2 <- flats[which((flats$District == "Ленинский")&flats$Id != 17&flats$Id != 79&flats$Id != 106&flats$Id != 378&flats$Id != 251&flats$Id != 384&flats$Id != 217&flats$Id != 317&flats$Id != 284&(flats$PlOb - flats$PlZ - flats$PlK)>0),] # todo

# Модель - благодаря функции printsum тестирует модель и отображает результаты
m2 <- printsum(lm(LnCena ~ log(PlOb - PlZ - PlK) + log(PlZ) + log(PlK) + log(CenterDistance), data=flats_m2))

# Другие параметры, немного уточняющие модель
# I(Elit * NkThree * DistrictFru) + I(Type*(1-(DistrictZav|DistrictLen))) + I(Elit * NkThree * DistrictFru) + 

# Выводит табличку, которая ниже
districts = levels(flats$District)[1:9]
tdistricts = matrix(0, nrow = length(districts), ncol = length(districts))
rownames(tdistricts) <- districts
colnames(tdistricts) <- districts
for (ii in 1:length(districts)) {
  for (jj in 1:length(districts)) {
    p = t.test(flats[which(flats$District == districts[ii]),]$LnCena, flats[which(flats$District == districts[jj]),]$LnCena)$p.value
    tdistricts[ii,jj] = p > 0.05
  }
}
print(tdistricts)

#              Заводской Ленинский Московский Октябрьский Партизанский Первомайский Советский Фрунзенский Центральный
# Заводской            1         0          0           0            0            0         0           0           0
# Ленинский            0         1          1           1            1            1         0           1           0
# Московский           0         1          1           1            0            0         0           1           0
# Октябрьский          0         1          1           1            1            1         0           1           0
# Партизанский         0         1          0           1            1            1         1           1           1
# Первомайский         0         1          0           1            1            1         1           1           1
# Советский            0         0          0           0            1            1         1           0           1
# Фрунзенский          0         1          1           1            1            1         0           1           0
# Центральный          0         0          0           0            1            1         1           0           1

#
#
# ФУНКЦИИ
#
#

# src: http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
# возвращает p-value из модели
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# отображает p модели, p членов, p тестов. *** помечает отклонения и в конце отображает TRUE/FALSE для этих условий
printsum <- function(m, p1=0.05, p2=0.05)
{
  t1 = summary(m)$coefficients[,4]
  for (i in 1:length(t1)) {
    names(t1)[i] = paste("model", names(t1)[i])
    if (!(t1[i] < p1)) {
      names(t1)[i] = paste(names(t1)[i], "***")
    }
  }
  t2 = mtest(m)
  for (i in 1:length(t2)) {
    if (!(t2[i] > p2)) {
      names(t2)[i] = paste(names(t2)[i], "***")
    }
  }
  x = getOption("scipen"); options(scipen=100)
  pv = lmp(m)
  names(pv) = "model p.value"
  if (!(pv < p1)) {
    names(pv) = paste(names(pv), "***")
  }
  print(pv)
  cat("\n")
  print(t1)
  cat("\n")
  options(scipen=x)
  print(t2)
  cat("\n")
  t3 = c(pv < p1, min(t2) > p2)
  names(t3) = c(paste("model <", p1), paste("tests >", p2))
  print(t3)
  cat("\nmulti-collinearity\n")
  print(vif(m2))
  
  return(m)
}

# тестирует модель и возвращает результаты тестов
mtest <- function(m)
{
  r = c()
  # тесты на нормальность распределения остатков
  p = jarque.test(residuals(m))$p.value ; names(p) = "Res.norm jarque"; r = c(r, p)
  p = pearson.test(residuals(m))$p.value; names(p) = "Res.norm pearson"; r = c(r, p)
  p = shapiro.test(residuals(m))$p.value; names(p) = "Res.norm shapiro"; r = c(r, p)
  
  # на автокорреляцию
  p = bgtest(m,order=1,type="F")$p.value    ; names(p) = "Autocor bgtest 1 F"; r = c(r, p)
  p = bgtest(m,order=1,type="Chisq")$p.value; names(p) = "Autocor bgtest 1 C"; r = c(r, p)
  p = bgtest(m,order=2,type="Chisq")$p.value; names(p) = "Autocor bgtest 2 C"; r = c(r, p)
  p = durbinWatsonTest(m)$p                 ; names(p) = "Autocor durbinWatson"; r = c(r, p)
  
  # проверка на гетероскедастичность остатков
  p = bptest(m)$p.value ; names(p) = "Geter. bptest"; r = c(r, p)
  p = ncvTest(m)$p      ; names(p) = "Geter. ncvTest"; r = c(r, p)
  
  return(r)
}

#
#
# ПРОЧЕЕ
#
#

# проверка распределения случайных ошибок регрессии
e=residuals(m2)
hist(e,breaks=15,col="lightblue",freq=FALSE)
xfit<-seq(min(e), max(e), length=40)
yfit<-dnorm(xfit, mean=mean(e), sd=sd(e))
lines(xfit, yfit, col="blue", lwd=2)
qqnorm(residuals(m2))
qqline(residuals(m2))

