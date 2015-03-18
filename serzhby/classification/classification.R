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

# linear regression model
reg = lm(rings ~ diameter, data = filtered)
summary(reg)
testModel(reg)
