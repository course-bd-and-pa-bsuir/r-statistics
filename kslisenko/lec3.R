## строим линейную регрессию
fit = lm(weight ~ height, data = women)
summary(fit)
## регрессия
fitted(fit)
## реальный вес
women$weight
## разница между данными регрессии и реальной модели
residuals(fit)
## предварительный анализ вида функциональной зависимости
## зависимость роста от веса
plot(women$height, women$weight)
abline(fit)
## график ошибок
plot(residuals(fit), type = "b")

## строим полиноминальную регрессию, нелинейную (y = a + b1*x + b2*x*x)
fit2 = lm(weight~height + I(height^2), data = women)
fitted(fit2)
summary(fit2)
lines(women$height, fitted(fit2))

## подбор кривых
library(car)
scatterplot(weight~height, data = women)
## зависимости между переменными
scatterplotMatrix(mtcars)

## зависимости расходов топлива от веса автомобиля - построить регрессию
plot(mtcars$mpg, mtcars$wt)
fit = lm(mpg ~ wt, data = mtcars)
summary(fit)
## линейная регрессия
plot(mtcars$wt, mtcars$mpg)
abline(fit)
## линейная и полиноминальная
scatterplot(mpg ~ wt, data = mtcars)

## зависимость расхода топлива от веса + лошадиных сил
fit = lm(mpg ~ wt + hp, data = mtcars)
summary(fit)
coef(fit) ## вывести коэффициенты регрессии
confint(fit) ## доверительные интервалы - истинное значение параметра лежит с вероятностью 95% в этом интервале

## гистограмма остатков
hist(residuals(fit), breaks = 10)
## построить на нормальной вероятностной бумаге
qqnorm(residuals(fit))
qqline(residuals(fit))
## построение на нормальной вероятностной бумаге - другим способом
qqPlot(fit)

## формальная проверка с помощью теста - являются ли остатки нормально распределёнными
library(moments)
## с помощью метода Жака Бера
jarque.test(residuals(fit))

## построить регрессию одного параметра от некоторых других - вариант 24
regression_data <- read.table("~/Documents/BD and PA Spring 2015/regression_data.csv", sep=";", dec=",", quote="\"")
## чтобы убрать размерность нужно прологарифмировать - чтобы получить прогнозные значения - нужно экспонировать

## зависимость дохода от оборотного капитала и количества работников
regression_data$LOG_DOHOD = log(regression_data$DOHOD)
regression_data$LOG_OBOROT_KAP = log(regression_data$OBOROT_KAP)
regression_data$LOG_RABOTNIKI = log(regression_data$RABOTNIKI)
regression_data$LOG_ISP_KAP = log(regression_data$ISP_KAP)
regression_data$LOG_KAPITALIZ = log(regression_data$KAPITALIZ)
regression_data$D15 = 0
regression_data$D15[14] = 1
edit(regression_data)
fit = lm(LOG_DOHOD ~ LOG_OBOROT_KAP + LOG_RABOTNIKI + LOG_ISP_KAP + LOG_KAPITALIZ + D15, data = regression_data)
fit = lm(LOG_DOHOD ~ LOG_OBOROT_KAP + LOG_RABOTNIKI, data = regression_data)
## остатки
plot(residuals(fit))
confint(fit)
summary(fit)
cor(regression_data)
scatterplotMatrix(regression_data)

## TODO попробовать последовательно добавлять параметры в регрессию, посмотреть после добавления какого параметра портится статистическая значимость