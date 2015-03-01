data1 <- read.csv("data/lab3_1.csv", header=FALSE)
data1 <- data1$V1

data2 <- read.csv("data/lab3_2.csv", header=FALSE)
data2 <- data2$V1

# Критерий Жака-Бера
# Соответствие распределения нормальному
# Если критерий более 0.05 - распределение нормальное
library("moments")
jarque.test(data1)
jarque.test(data2)

# Графическая интерпретация
par(mfrow=c(1,2))
qqnorm(data1)
qqline(data1)

qqnorm(data2)
qqline(data2)
#==============

# Критерий Пирсона
library("nortest")

pearson.test(data1)
pearson.test(data2)

par(mfrow=c(1,2))
hist(data1, breaks=30)
hist(data1, breaks=30)
#==============

# Корреляция
s <- state.x77

cov(s)
cor(s)

# Визуализация
library(corrgram)

corrgram(s)
# с пирогами
corrgram(s, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="")


# Проверка корелляций
cor.test(s[, 2], s[, 3])
cor.test(s[, 2], s[, 4])
#=============

# Таблицы сопряжённости
library(vcd)
library(stats)

att <- Arthritis
head(att)

# Таблица сопряжённости
tab <- table(att$Improved)
prop.table(tab) # доли соотношений

tab <- xtabs(~Treatment+Improved, data=att)
tab # сопряжённость варианта лечения и результата
ftable(prop.table(tab)) # в долях соотношений

chisq.test(tab) # признаки зависимы, H0 о независимости отвергается
#=============

# fisher.test(tab)
# assocstats(tab)
