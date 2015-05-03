# Energy efficiency dataset classification: https://archive.ics.uci.edu/ml/datasets/Energy+efficiency

data <- read.csv('./data/ENB2012_data_orig.csv', header=TRUE)

data$Y1 <- round(data$Y1, digits=0) # For classification with multiple classes
data$Y1 <- factor(data$Y1) # For neural networks
data <- rbind(data[,c(1,2,3,4,5,6,7,8,9)])

# 85% of the sample size
sample.size <- floor(0.85 * nrow(data))

# set the seed to make partition reproductible
set.seed(12345)
train.ind <- sample(nrow(data), size = sample.size)

train <- data[train.ind, ]
test <- data[-train.ind, ]

classificationTest <- function(pred, real) {
  print("Matrix of equality (predicted == real)")
  guessed <- (real == pred)
  print(guessed)
  
  print("Total equal:")
  print(length(guessed[guessed == TRUE]))
}

# Classification Tree with rpart
library(rpart)

# grow tree 
fit <- rpart(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=train, method="class")
# and prune
fit1 <- prune(fit, cp=0.01)

printcp(fit1) # display the results 
plotcp(fit)   # visualize cross-validation results 
summary(fit)  # detailed summary of splits
par(mfrow=c(1,1))
# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for data")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

ypred <- predict(fit1, test, type="class")
yreal <- test$Y1

classificationTest(ypred, yreal) # 67 equal values


# Neural network
library(nnet)

nnmodel <- nnet(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=train, size=length(levels(data$Y1)), rang=0.1, decay=0.05, maxit=500)
summary(nnmodel)

ypred=predict(nnmodel, test, type="class")
yreal<-test$Y1

classificationTest(ypred, yreal) # 63 equal values


# K-means clustering
kmodel <- kmeans(train[,c(1,2,3,4,5,6,7,8)], length(levels(data$Y1)), nstart=25) # 37 clusters, BSS/TSS -> 100%
kmodel
# Predictions
library(clue)
table(levels(data$Y1)[cl_predict(kmodel, test[,c(1,2,3,4,5,6,7,8)])], test$Y1)
