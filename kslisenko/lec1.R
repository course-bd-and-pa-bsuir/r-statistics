# установка пакетов
install.packages("psych")

## описательная статистика
library(pastecs)
stat.desc(mtcars$mpg)

## коэффициенты ассиметрии и эксцесса
library(psych)
describe(mtcars)
describeBy(mtcars, list(cyl=mtcars$cyl))

## визуализация - демо какие бывают графики
demo(graphics)

## диаграмма размахов
boxplot(mtcars$mpg, main="название диаграммы", ylab = "название оси y")

## зависимость расхода топлива от количества цилиндров - диаграммы размахов
boxplot(mtcars$mpg~mtcars$cyl)

## добавляем новые столбцы, которые вводят метки для разных значений
mtcars$cyl.f <- factor(mtcars$cyl,levels=c(4,6,8),labels=c("4","6","8"))
mtcars$am.f <- factor(mtcars$am,levels=c(0,1),labels=c("авто","ручная"))

## строим boxplot "Расход топлива у разных типов машин"
boxplot(mpg ~ am.f * cyl.f, data = mtcars, varwidth=TRUE, col =c("gold", "darkgreen"),
        main="Расход топлива у разных типов машин", xlab = "Тип автомобиля")

# вывод датасета
mtcars

# выброс (резко отличающееся наблюдение)
boxplot(mtcars$qsec)

# точечная диаграмма
dotchart(mtcars$mpg, labels=row.names(mtcars), cex = .6, main="Расход топлива в зависимости от марки машины", xlab = "расход")

# точечная диаграмма с отсортированными, сгрупированными и раскрашенными значениями
x = mtcars[order(mtcars$mpg),]
## фактор, но можно и без него
x$cyl = factor(x$cyl) 
x$color[x$cyl==4] = "red"
x$color[x$cyl==6] = "blue"
x$color[x$cyl==8] = "darkgreen"
dotchart(x$mpg, labels=row.names(x), cex = .7, groups = x$cyl, gcolor = "black", 
         color = x$color, rch = 19, main = "Расход топлива для разных марок автомобилей,
         сгруппированных по числу цилиндров", xlab = "миль на галлон")

## дефолтные параметры для всех типов диаграмм
par()

## зависимость одного параметра от другого
plot(mtcars$wt, mtcars$mpg, main="расход топлива в зависимости от веса машины", xlab = "вес",
     ylab="Расход топлива", pch=18, col="blue")

## гистограмма
hist(mtcars$mpg)
h = hist(mtcars$mpg, breaks=12, col="red", xlab = "Расход топлива", main = "Цветная гистограмма с 12 столбцами")
## вывод свойств отдельно
h

## гистограмма с кривой нормального распределения и в рамочке
x = mtcars$mpg;
h = hist(x, breaks=12, col="red", xlab = "Расход топлива", main = "Цветная гистограмма с 12 столбцами");
1 ## любой символ толкаем в стандартный ввод
xfit <- seq(min(x), max(x), length=40);
yfit = dnorm(xfit, mean = mean(x), sd = sd(x));
yfit = yfit * diff(h$mids[1:2]) * length(x);
lines(xfit, yfit, col="blue", lwd=2);
box()

## корреляционное поле с линией регрессии
plot(mtcars$wt, mtcars$mpg);
1
abline(lm(mtcars$mpg ~ mtcars$wt))
title("Regression of MPG on weight")

## вывод 4 диаграмм
par(mfrow = c(2,2))
plot(mtcars$wt, mtcars$mpg, main = "Название Д1"); 
1
plot(mtcars$wt, mtcars$disp, main = "Название Д2"); 
hist(mtcars$wt, main = "Название Д3"); 1
boxplot(mtcars$wt, main="Название Д4"); 1 
par(opar)