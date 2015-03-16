# Практическое занятие, 10
# Подготовка данных для задач классификации

# X1 Relative Compactness 
# X2 Surface Area 
# X3 Wall Area 
# X4 Roof Area 
# X5 Overall Height 
# X6 Orientation 
# X7 Glazing Area 
# X8 Glazing Area Distribution 
# y1 Heating Load 
# y2 Cooling Load
data <- read.csv('./data/ENB2012_data_corrupted.csv', header=TRUE)
data_orig <- read.csv('./data/ENB2012_data_orig.csv', header=TRUE)

# Посмотрим вскользь на графики зависимостей
par(mfrow=c(3,2))

for(col in c(1, 2, 7)) {
  plot(data$Y1~data[, col], xlab=col)
  plot(data$Y2~data[, col], xlab=col)
}

# Гистограммы
for(col in c("X1", "X2", "X7", "Y1", "Y2")) {
  hist(data[, col], breaks=20, main=col)
}

# Функция для сравнения наборов данных
# Строит boxplot'ы попарно для каждой колонки, сравнивает средние значения
compare.datasets <- function(old.data, new.data, cols) {
  par(mfrow=c(ceiling(length(cols)/2), 2))
  for(col in cols) {
    boxplot(old.data[, col], new.data[, col], main=col, names=c("old", "new"))
    meanD = mean(old.data[, col], na.rm=TRUE)
    meanN = mean(new.data[, col])
    print(paste(meanD, ", new: ", meanN, ", err%: ", (1 - meanD/meanN)*100))
  }
}


# Строки с пропущенными значениями:
data[!complete.cases(data),]

# 1. Можно выкинуть такие строки:
newdata <- na.omit(data)
compare.datasets(data, newdata, c("X1", "X2", "X7", "Y1", "Y2"))

# Но лучше воспользоваться инструментами множественной замены:
# 2. Amelia
library(Amelia)
newdata <- amelia(data)
summary(newdata)
plot(newdata)
newdata <- newdata$imputations$imp5
compare.datasets(data, newdata, c("X1", "X2", "X7", "Y1", "Y2"))

# 3. ...


# Необходимо убрать аномалии (outliers)
# 1. Заменить средним
library(outliers)
newdata1 <- rm.outlier(newdata, fill=TRUE, median=FALSE)
compare.datasets(data, newdata1, c("X1", "X2", "X7", "Y1", "Y2"))

# 2. Заменить медианным
newdata1 <- rm.outlier(newdata, fill=TRUE, median=TRUE)
compare.datasets(data, newdata1, c("X1", "X2", "X7", "Y1", "Y2"))

# 3. Если выбросы остались, то лучше не делать лишние проходы - они увеличивают разброс средних
# Лучше удалить оставшиеся аномалии:
rm.outlier <- function(dataset, col) {
  outlier_tf <- outlier(dataset[, col], logical=TRUE)
  find_outlier <- which(outlier_tf==TRUE, arr.ind=TRUE)
  return(dataset[- find_outlier,])
}
newdata2 <- rm.outlier(newdata1, "X1")
newdata2 <- rm.outlier(newdata2, "X1")
compare.datasets(data, newdata2, c("X1", "X2", "X7", "Y1", "Y2"))

# Посмотрим вскользь на графики зависимостей
par(mfrow=c(3,2))

for(col in c(1, 2, 7)) {
  plot(newdata2$Y1~newdata2[, col], xlab=col)
  plot(newdata2$Y2~newdata2[, col], xlab=col)
}

# Вроде норм
write.csv(newdata2, file='./data/ENB2012_data_corrected.csv', row.names = FALSE, col.names=TRUE)
