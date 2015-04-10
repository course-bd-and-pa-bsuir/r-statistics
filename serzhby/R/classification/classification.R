setwd('~/code/R/BDPA/r-statistics/serzhby/classification/')
source('prepare_data.R')
source('../test_lm.R')

library(corrgram)

# dataset = read.csv('abalone.data.txt', sep = ',')
# dataset = insertNA(dataset)
# write.csv(dataset, file = 'abalone.data.na.txt')

dataset = read.csv('abalone.data.na.txt', sep = ',')
attach(dataset)

# deal with na

plot(length)
# select rows where length have na
nas = dataset[is.na(length),]
# plots all NAs
points(nas$X, rep(c(0.2), times = length(nas$X)), col = 'red')

detach(dataset)
# remove all rows where any parameter equals NA
filtered = removeNA(dataset)

# remove outliers
noOutliers = filtered

filterOutliers = function(model, column) {
  outs = boxplot(column)$out
  model = model[!column %in% outs, ]
}

noOutliers = filterOutliers(noOutliers, noOutliers$length)
noOutliers = filterOutliers(noOutliers, noOutliers$diameter)
noOutliers = filterOutliers(noOutliers, noOutliers$height)
noOutliers = filterOutliers(noOutliers, noOutliers$wholeWeight)
noOutliers = filterOutliers(noOutliers, noOutliers$shuckedWeight)
noOutliers = filterOutliers(noOutliers, noOutliers$visceraWeight)
noOutliers = filterOutliers(noOutliers, noOutliers$shellWeight)

boxplot(noOutliers$length)
boxplot(noOutliers$diameter)
boxplot(noOutliers$height)
boxplot(noOutliers$wholeWeight)
boxplot(noOutliers$shuckedWeight)
boxplot(noOutliers$visceraWeight)
boxplot(noOutliers$shellWeight)

# linear regression model
noOutliers$n = noOutliers$shuckedWeight + noOutliers$visceraWeight + noOutliers$shellWeight
noOutliers$diff = noOutliers$wholeWeight / noOutliers$n
r1 = lm(wholeWeight ~ n, data = noOutliers)
r2 = lm(wholeWeight ~ shuckedWeight + visceraWeight, data = noOutliers)
r3 = lm(log(wholeWeight) ~ log(shuckedWeight) + log(visceraWeight) + log(shellWeight) + sex, data = noOutliers)
r4 = lm(log(length) ~ log(diameter) + log(height), data = noOutliers)
good = FALSE
good = good | testModel(r1)
good = good | testModel(r2)
good = good | testModel(r3)
good = good | testModel(r4)
print(good)

# cannot build any good regression model

# try to restore wholeWeight from another weights

# try to impute using knn
mat = data.matrix(dataset)
imputed = impute.knn(mat)


##################################################
## Classification
##################################################

library(rpart)
library(nnet)
library(e1071)
library(kknn)
library(pROC)

# read original dataset for now, but should use prepared data instead
dataset = read.csv('abalone.data.txt', sep = ',')
#dataset$rings = as.character(dataset$rings)
# generate sample datasets
trainSamplePercent = 0.9
dataLength = length(dataset[, 1])
sampleIndeces = sample(dataLength, trainSamplePercent * dataLength, replace = FALSE)
#sampleIndeces = sample(dataLength, 100, replace = FALSE)
trainData = dataset[sampleIndeces, ]
testData = dataset[-sampleIndeces, ]

form = rings ~ .


testClassificator = function(model, testData, columnIndex) {
  yhat2 = predict(model, testData)
  y <- testData[, 9]
  SVMTest = sum(sqrt( (yhat2-y)^2) / length(y) )
  print(SVMTest)
  rocobj <- plot.roc(y, yhat2, main = "Confidence intervals", percent = TRUE, ci = TRUE,  print.auc = TRUE) 
}


## SVM ##
# train
model <- svm(form, data = trainData, cost = 0.03, gamma = 1, coef0 = 10, kernel = "polynomial")
preds <- predict(model, trainData)

# check
testClassificator(model, testData, 9)

## Neural Networks ##
# train
ep <- 0.001
model <- nnet(form, data = trainData, size = 3, rang = ep, decay = 0.0001, maxit = 400)
testClassificator(model, testData, 9)

## ecursive Partitioning
trainData.tr <- rpart(form, data = trainData, cp = 0.017)
model = prune(trainData.tr, cp = 0.016)

testClassificator(model, testData, 9)

## Random Forest
library(randomForest)
model <- randomForest(form, data = trainData)
# print(model) # view results 
# importance(model) # importance of each predictor

testClassificator(model, testData, 9)
