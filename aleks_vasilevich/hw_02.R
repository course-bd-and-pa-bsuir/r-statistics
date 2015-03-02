#Homework_02

car = mtcars;

#1. Построить корреляционную матрицу для количественных переменных mtcars
cor(car)

#2. Выбрать любую пару и проверить значимость коэффициента корреляции
# cyl cor() against wt
cor(car[,c(2,6)])
# mpg cor() against hp
cor(car[,c(1,4)])

att = Arthritis
head(att,3)
