# Домашнее задание
# Вычислить описательные статистики для всех переменных из state.region
# Построить графики!

# state.region:
#   factor giving the region (Northeast, South, North Central, West) that each state belongs to.
# state.x77:
#   matrix with 50 rows and 8 columns giving the following statistics in the respective columns.
# Population:
#   population estimate as of July 1, 1975
# Income:
#   per capita income (1974)
# Illiteracy:
#   illiteracy (1970, percent of population)
# Life Exp:
#   life expectancy in years (1969–71)
# Murder:
#   murder and non-negligent manslaughter rate per 100,000 population (1976)
# HS Grad:
#   percent high-school graduates (1970)
# Frost:
#   mean number of days with minimum temperature below freezing (1931–1960) in capital or large city
# Area:
#   land area in square miles

library(pastecs)
library(psych)

df <- as.data.frame(state.x77)
df$Region <- state.region

#
stat.desc(df[c(1:8)])

describe(df[c(1:8)])
describeBy(df[c(1:8)], list(Region=df$Region))

# 
aggregate(df[c(1:8)], by=list(region=df$Region), mean)
aggregate(df[c(1:8)], by=list(region=df$Region), median)

# 
dfx <- df[order(df$Income),]
dfx$Color[dfx$Region == "Northeast"] <- "red"
dfx$Color[dfx$Region == "South"] <- "blue"
dfx$Color[dfx$Region == "North Central"] <- "orange"
dfx$Color[dfx$Region == "West"] <- "green"
dotchart(dfx$Income, labels=row.names(dfx), cex=0.5, groups=dfx$Region, gcolor="black", color=dfx$Color, pch=19, main="Доход по штатам (группировка по регионам)", xlab="Доход")

# 
plot(df$Income, df$Population, main="Доход от численности населения", xlab="Доход", ylab="Численность населения", pch=19, col="blue", cex=0.5)
text(df$Income, df$Population, labels=row.names(df), cex=0.6, pos=4, col="darkgreen")

#
plot(df$"Life Exp", df$Murder, main="Продолжительность жизни от количества убийств", xlab="Продолжительность жизни", ylab="Убийств", pch=19, col="blue", cex=0.5)
abline(lm(df$Murder ~ df$"Life Exp"))

#
dfx <- df$"HS Grad"
dfh <- hist(dfx, breaks=20, col="darkgreen", xlab="Процент населения с высшим образованием", main="Гистограмма населения с высшим образованием\nи кривой нормального распределения")
xfit <- seq(min(dfx), max(dfx), length=40)
yfit <- dnorm(xfit, mean=mean(dfx), sd=sd(dfx))
yfit <- yfit * diff(dfh$mids[1:2]) * length(dfx)
lines(xfit, yfit, col="yellow", lwd=2)
box()

#
par(mfrow=c(1,2))
boxplot(df$Population~df$Region, main="Популяция", cex.axis=0.4)
boxplot(df$Income~df$Region, main="Доход", cex.axis=0.4)
par(mfrow=c(1,1))
title("ПО РЕГИОНАМ")

