m <- mtcars

summary(m[, c(1,3)])

#======

library(pastecs)
stat.desc(m[, c(1,3)])

#======

# Коэффициенты ассиметрии и эксцесса
# skew - коэф. ассиметрии
# kurtosis - эксцесс
library(psych)
describe(m)
describeBy(m, list(cyl=m$cyl))
describeBy(m, list(cyl=m$cyl, am=m$am))

#======

# Вычисление описательных статистик
aggregate(m[, c(3,4)], by = list(am=m$am), mean)

# Визуализация
# Диаграммы размахов
boxplot(m$mpg, main="Расход топлива (MPG)", ylab="Значение", xlab="Машины")
boxplot(m$mpg~m$cyl, main="Расход по количеству цилиндров")

m$cyl.f <- factor(m$cyl, levels=c(4,6,8), labels=c('4', '6', '8')) # Добавляем столбец с фактором по цилиндрам
m$am.f <- factor(m$am, levels=c(0,1), labels=c("авто", "ручная"))
boxplot(mpg ~ am.f * cyl.f, 
        data=m, 
        varwidth=TRUE, 
        col=c("gold", "darkgreen"), 
        main="Расход топлива у различных типов машин", 
        xlab="Тип автомобиля")

# Выбросы с усами
boxplot(m$qsec)

#======

# Точечная диаграмма
dotchart(m$mpg, main=c('Расход топлива по машинам'), labels=row.names(m), xlab='Расход')

# Точечная диаграмма с отсортированными, сгрупированными и раскрашенными значениями
x <- m[order(m$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, 
         labels=row.names(x), 
         cex=.7, 
         groups=x$cyl, 
         gcolor="black", 
         color=x$color, 
         pch=19, 
         main="Расход топлива", 
         xlab="Миль на галлон")

#=======

# Диаграмма рассеяния (корреляционное поле)
plot(m$wt, m$mpg,
     main="Расход топлива от веса",
     xlab="Вес", 
     ylab="Расход топлива",
     pch=18, 
     col="blue")

# С линией регрессии
plot(m$wt, m$mpg)

abline(lm(m$mpg ~ m$wt))
title("Линия регрессии")
#=======

# Гистограмма
h <- hist(m$mpg,
     breaks=12,
     col="red",
     xlab="Расход топлива",
     main="Гистограмма расхода топлива на 12 диапазонов")

# С кривой распределения и в рамочке
x <- m$mpg
h <- hist(x,
          breaks=12,
          col="red",
          xlab="Расход",
          main="Гистограмма расхода топлива")

xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()
#=======


# Вывод нескольких графиков в одной области
par(mfrow=c(2,2))
plot(m$wt, m$mpg, main="Название 1")
plot(m$wt, m$disp, main="Название 2")
hist(m$wt, main="Название 3")
boxplot(m$wt, main="Название 4")
