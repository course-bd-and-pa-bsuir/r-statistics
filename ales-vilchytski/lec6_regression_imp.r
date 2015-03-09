# Лекция 6 - улучшение и оценка качества регрессионных моделей

library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(gplots)
library(MASS)
library(ggplot2)

m2 <- lm(mpg ~ wt + hp, data=mtcars)
m3 <- lm(mpg ~ wt + hp + disp, data=mtcars)

# Определение улучшения качества модели при вводе новых факторов
# RSS - объяснённая дисперсия для моделей
# F - статистика мала
# p > 0.05 - H0 верна, введение новых переменных не улучшает качество модели
anova(m2, m3)

# Дисперсионный анализ модели
# Сумма квадратов показывает "вклад" каждого параметра
anova(m2)

# Информационное сравнение моделей
# Лучше та модель, у которой значение критерия меньше
AIC(m2, m3)

# Тест Стьюдента одновыборочный
# p > 0.05 - Гипотеза о равенстве среднего значения выбранному не отвергается
# ! используется при нормальном распределении выборки 
t.test(mtcars$mpg, mu=20)

# Тест Уилкоксона одновыборочный
# p > 0.05 - Гипотеза о равенстве среднего значения выбранному не отвергается
wilcox.test(mtcars$mpg, mu=20)

# p > 0.05, данные распределены нормально
pearson.test(mtcars$mpg)


# Тест Стьюдента для независимых выборок
# p < 0.05 -> H0 отвергается, есть различия выборок
t.test(mtcars$mpg~mtcars$am)

# Тест Уилкоксона для независимых выборок
# p < 0.05 -> H0 отвергается, есть различия выборок
wilcox.test(mtcars$mpg~mtcars$am)
# по умолчанию дисперсии не равны *var.equal=TRUE

# для зависимых выборок
t.test(mtcars$cyl, mtcars$am, paired=TRUE)


stripchart(mtcars$mpg~mtcars$cyl)
boxplot(mtcars$mpg~mtcars$cyl)
plotmeans(mtcars$mpg~mtcars$cyl)

# Непараметрические тесты множественных сравнений
# Вывод - имеются различия (p-value < 0.05)
kruskal.test(mtcars$mpg~mtcars$cyl)

