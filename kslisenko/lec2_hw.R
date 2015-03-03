library(pastecs)

states = as.data.frame(state.x77)
states$Region = state.region
## описательная статистика
summary(states)
## диаграмма размахов по населениям штатов
boxplot(states$Population, main = "Население штатов", ylab = "Население")
## Диаграмма размахов по штатам и доходам
boxplot(states$Income, main = "Доходы в штатах", ylab = "Доход")
## Зависимость продолжительости жизни от доходов
boxplot(states$Income ~ states$'Life Exp')

## считаем сумму населения по регионам
regionPopulation = aggregate(states$Population, by = list(states$Region), FUN = sum)
## считаем средний доход по регионам
regionAverageIncome = aggregate(states$Income, by = list(states$Region), FUN = mean)

## зависимость населения от территории
plot(states$Population ~ states$Area, ylab ="Население", xlab = "Территория")

par(mfrow = c(1,2))
plot(regionPopulation, main = "Население по регионам", ylab = "Население")
plot(regionAverageIncome, main = "Ср.доход по регионам", ylab = "Ср. доход")
par(mfrow = c(1,1))

## Корреляция населения и дохода
plot(regionPopulation$x ~ regionAverageIncome$x)
abline(lm(regionPopulation$x ~ regionAverageIncome$x))

## среднее значение параметров по регионам
aggregate(states$Income, by = list(region = states$Region), mean)
aggregate(states$Population, by = list(region = states$Region), mean)

## Население по регионам
dotchart(states$Income, main = "Доход по штатам", labels = row.names(states))

## гистограмма убийств по штатам
hist(states$Murder)

## Оцениваем, нормальное ли распределение даходов в штатах
library(moments)
jarque.test(states$Income)
## график на вероятностной бумаге
qqnorm(states$Income)
qqline(states$Income)

## гистограмма доходов по штатам
hist(states$Income)

## проверка на критерий Пирсона
library(nortest)
pearson.test(states$Income)

## корреляция
cor(states$Income, states$Murder)
