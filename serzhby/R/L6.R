m2 = lm(mpg ~ wt + hp, data = mtcars)
m3 = lm(mpg ~ wt + hp + disp, data = mtcars)
s3d$plane3d(m2)

# внутри теста - сравнение дисперсий. тест на значимость новых переменных
anova(m2, m3)
# введение новых переменных не влияет. p-value > 5%, значит не влияет.

# дисперсионный анализ
anova(m2)
# Sum Sq показывает уровень влияния на результат.

summary(m3)
# подтверждает, что disp незначима

AIC(m2, m3)
# выбираем ту, где значение AIC меньше

# тест стьюдента. предполагаем, что распределение нормальное, но вообще надо проверять
t.test(mtcars$mpg, mu=20)
# p-level больше 5%, значит принимается гипотеза о том, что расход не равен 20

t.test(mtcars$mpg, mu=19)

# ещё один тест на равенство средних
wilcox.test(mtcars$mpg, mu=20)

# проверим на нормальность
pearson.test(mtcars$mpg)

# тесть стьюдента для независимых
t.test(mtcars$mpg ~ mtcars$am)
# тесть уилкокса
wilcox.test(mtcars$mpg ~ mtcars$am)
# p-value < 5%. результат - есть влияние коробки передач на расход

# как использовать для зависимых
t.test(mtcars$cyl, mtcars$am, paired = TRUE)
stripchart(mtcars$mpg ~ mtcars$cyl)
plotmeans(mtcars$mpg ~ mtcars$cyl)

# анализирует разницу средних значений
kruskal.test(mtcars$mpg ~ mtcars$cyl)
