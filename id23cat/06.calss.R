# есть ли достоверное влияние одной величины от другой
m2=lm(mpg~wt+hp,data=mtcars)
m3=lm(mpg~wt+hp+disp,data=mtcars)
# тест на значимость новых переменных
# существенно ли увеличилось качество при добавлении 
anova(m2,m3)
# F -статистика , Н0 не отвергается, введение доп переменной не улучшает качество модели

summary(m2)
summary(m3)

# AIC сравнение удавшихся моделей
# выбирается модель с меньшим значением
AIC(m2,m3)

# дисперсионный анализ модели
# вклад веса оказывает большее влияние на разброс расхода топлива
anova(m2)


#тест Стьюдента
t.test(mtcars$mpg, mu=20)
# 25 отверается
t.test(mtcars$mpg, mu=25)
# 18 отверается
t.test(mtcars$mpg, mu=18)
# интервал 17.91768 22.26357
t.test(mtcars$mpg, mu=17)
# тест Уилкоксона
# все отлично


wilcox.test(mtcars$mpg,mu=20)

# данные нормально распределены
pearson.test(mtcars$mpg)

head(mtcars)


# выборочные сравнения Стьюдента
# отвергаем, 
t.test(mtcars$mpg~mtcars$am)
# доверительный интервал -- для разницы между средними

# Для Уилклксона такой же результат -- есть различия
wilcox.test(mtcars$mpg~mtcars$am)

t.test(mtcars$mpg~mtcars$am, paired=TRUE)

boxplot(mtcars$mpg~mtcars$cyl)
plotmeans(mtcars$mpg~mtcars$cyl)

# сушествует достоверное различие
kruskal.test(mtcars$mpg~mtcars$cyl)
