student.mat <- read.csv("~/Documents/BD and PA Spring 2015/Students in school/student/student-mat.csv", sep=";")
student.por <- read.csv("~/Documents/BD and PA Spring 2015/Students in school/student/student-por.csv", sep=";")

allData = rbind(student.mat, student.por)

## случайным образом иммитируем пустые места в данных
addEmptyPlacesToData = function(data) {
  errorDataPercents = 0.9  ## берём 90% данных, в которых делаем ошибки
  
  ## количество строк в таблице
  rows <- nrow(data)
  ## генерируем номера строк, в которые будем вносить проблемы
  errorRowsIndexes <- sample(1:rows, rows * errorDataPercents)
  ## делим датасет на 2 части, в 90% части делаем ошибки
  errorData <- data[errorRowsIndexes,]
  errorDataRows <- nrow(errorData)
  errorDataColumns <- ncol(errorData)
  ## ошибки будут в 10% колонок
  problemsCount <- as.integer(errorDataRows * errorDataColumns / 10)
  ## генерируем координаты проблем
  problemRowNumbers <- sample(1:errorDataRows,problemsCount,replace = TRUE)
  problemColumnNumbers <- sample(1:errorDataColumns,problemsCount,replace = TRUE)
  ## вносим все проблемы
  for(n in 1:problemsCount){
    errorData[problemRowNumbers[n],problemColumnNumbers[n]] <- NA
  }
  errorData
}

View(addEmptyPlacesToData(student.mat))
View(addEmptyPlacesToData(student.por))

## идеи что можно сделать:
## 1. Определяем безнадёжные данные, например где неизвестна оценка по всем трём тестам (или по двум)
## 2. Для некоторых данных можно брать среднее значение (например среднее значение теста по школе)
## 3. Построить корреляционную матрицу, посмотреть какие столбцы от каких могут зависеть (pairs, scatterplot)
## например если расстояние до дома большое, то reason это врятли "что близко к дому"
## 4. Строим линейную регрессию (на параметрах, которые зависят друг от друга, а это определим через pairs)
## 5. Можно попробовать дерево решений = decigion tree
## 6. Randon forest - строит очень много маленьких деревьев