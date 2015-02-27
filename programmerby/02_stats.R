# Classwork
###########

#
x = read.csv("02_data_vector.csv")

# Критерий Жака-Бэра
library(moments)
jarque.test(x[,1])

#
qqnorm(x[,1])
qqline(x[,1])

# Критерий согласия Пирсона (Хи-квадрат)
library(nortest)
pearson.test(x[,1])

#
df = as.data.frame(state.x77)

# Ковариация
cov(df)

# Корреляция
cor(df)
cor(df, method="spearman")

# 
library(corrgram)
# Коррелограмма
corrgram(df)
corrgram(df,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,text.panel=panel.txt,main="****")

# Проверка значимости корреляции
cor.test(df[,2], df[,4])

# Данные тестирования препарата на пациентах
library(vcd)
att = Arthritis
tab = table(att$Improved)
prop.table(tab)

tab2 = xtabs(~Treatment+Improved+Sex, data=att)
ftable(tab2)
ftable(prop.table(tab2,c(1,2)))

addmargins(tab2)

tab3 = xtabs(~Treatment+Improved, data=att)
chisq.test(tab3)

# df - степень свободы degree of freedom

# Тест Фишера
fisher.test(tab3)

# Показатели связи
assocstats(tab3)

# Задание:
# Построить корр. матрицу для количественных переменных mtcars
# Выбрать пару и проверить значимость коэф корреляции
# Построить таблицу сопряженности обыкновенных и относительных частот
# Есть ли связь между этими переменными?

mf = as.data.frame(mtcars)

corrgram(mf)
cor(mf)

cor.test(mf$wt, mf$qsec)

