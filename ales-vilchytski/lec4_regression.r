# Лекция 4 - регрессионный анализ

# Данные для парного регрессионного анализа
data <- women

fit <- lm(weight~height, data=data) # fit - функциональная завимость

# Intercept - константа, height - коэффициент при height
# Estimate - оценка
# Pr(>|t|) - p-value, значение менее доверительного интервала (5%) свидетельствует о наличии зависимости
# Residual standard error - ошибка модели, для сравнения с другими моделями
# Multiple R-squared - доля объяснённой части переменной (в данном случае 99% - очень большое значение)
# Adjusted R-squared полезен при сравнении моделей (выше - лучше)
summary(fit)

# Предсказанные значения по зависимости
fitted(fit)

# Ошибки модели (остатки)
residuals(fit)
plot(residuals(fit), type="b") # График показывает наличие неточности

# график зависимости выглядит линейным
plot(data$height, data$weight)
abline(fit)

# Уточнение модели вводом дополнительного члена полинома
fit2 = lm(weight~height+I(height^2), data=data)

# Модель более точна (уменьшилась ошибка, увеличился Adjusted R-squared, уменьшился p-value)
summary(fit2)

# Добавление квадратичного члена увеличило точность
plot(data$height, data$weight)
lines(data$height, fitted(fit2))

#=============


# Предсказание вида модели регрессии
# Зелёная линия - подгонка линейной регрессии
# Красная линия - полиномиальная регрессия
library(car)
scatterplot(weight~height, data=data)
scatterplotMatrix(mtcars) # каждый с каждым

# Модель регрессии от нескольких параметров - зависимость расхода от веса и мощности
carsFit <- lm(mtcars$mpg~(mtcars$wt+mtcars$hp))
summary(carsFit)
coef(carsFit) # коэффициенты модели
confint(carsFit) # Доверительные интервалы

# Проверка остатков модели
residuals(carsFit)
plot(residuals(carsFit), type="b")

hist(residuals(carsFit), breaks=20) # гистограмма остатков
# Оценка соответствия нормальному распределению
qqnorm(residuals(carsFit))
qqline(residuals(carsFit))
