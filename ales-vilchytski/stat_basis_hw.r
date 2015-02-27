data <- as.data.frame(state.x77)
data$Region = state.region

# Текстовые описательные характеристики
library(pastecs)
stat.desc(data[, -9])

library(psych)
describe(data[, -9])

# Среднее значение параметров по регионам
aggregate(data[, -9], by = list(region=data$Region), mean)
# медианное значение
aggregate(data[, -9], by = list(region=data$Region), median)

# =========
par(mfrow=c(1,1))
# Визуализация
# Точечная диаграмма
dotchart(m$mpg, main=c('Расход топлива по машинам'), labels=row.names(m), xlab='Расход')

# Точечная диаграмма с отсортированными, сгрупированными и раскрашенными значениями
x <- data[order(data$Murder),]
x$Region <- data$Region
x$color[x$Region == "Northeast"] <- "red"
x$color[x$Region == "South"] <- "blue"
x$color[x$Region == "West"] <- "darkgreen"
x$color[x$Region == "North Central"] <- "gold"
dotchart(x$Murder, 
         labels=row.names(data), 
         cex=.7,
         groups=x$Region, 
         gcolor="black", 
         color=x$color, 
         pch=19, 
         main="Убийства по регионам", 
         xlab="Кол-во убийств на душу населения")

# Диаграмма рассеяния (корреляционное поле)
plot(data$Income, data$Murder,
     main="Доход от количества убийств",
     xlab="Доход", 
     ylab="Количество убийств",
     pch=18,
     col="blue")

# Гистограмма
h <- hist(data$Area,
          breaks=20,
          col="blue",
          xlab="Площадь",
          main="Гистограмма площади на 20 диапазонов")

# С кривой распределения и в рамочке
x <- data$Frost
h <- hist(x,
          breaks=20,
          col="blue",
          xlab="Безработица",
          main="Гистограмма безработицы")

xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col="red", lwd=2)
box()

# Диаграммы размахов для некоторых параметров в разрезе регионов
mfrow_ <- par('mfrow')

par(mfrow=c(2,2))
boxplot(data$Population ~ data$Region, main="Популяция по регионам")
boxplot(data$Income ~ data$Region, main="Доход по регионам")
boxplot(data$Murder ~ data$Region, main="Убийства по регионам")
boxplot(data[,7] ~ data$Region, main="Высшее образование по регионам")

par(mfrow=mfrow_)
# =========
