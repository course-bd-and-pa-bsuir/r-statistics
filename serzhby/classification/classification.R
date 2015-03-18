setwd('~/code/R/BDPA/r-statistics/serzhby/classification/')
source('prepare_data.R')
source('../test_lm.R')

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

# try to impute using knn
mat = data.matrix(dataset)
imputed = impute.knn(mat)
