# Провести регрессионный анализ, найти оптимальные модели для учебных примеров

data <- read.csv("data/lab4_24.csv", header=TRUE)

scatterplotMatrix(data)

# большой p-value при коэффициентах и малый при модели свидетельствует об автокорреляции
fit <- lm(data$y~(data$x1+data$x2+data$x3+data$x4)) 
summary(fit)


