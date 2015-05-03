# Data preparation for classification

# Energy efficiency dataset: https://archive.ics.uci.edu/ml/datasets/Energy+efficiency
# Dataset has no problems, so make them

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


# For simplicity - use just some factors
Xcols = c("X1","X2","X7")
Ycols = c("Y1","Y2")
cols = c(Xcols, Ycols)

# General statistics
summary(data[,cols])
summary(data_orig[,cols])

par(mfrow=c(3,2))
# Dots:
for (col in cols) {
  plot(data[, col], main=col)
}

# Correlations
for(xcol in Xcols) {
  for(ycol in Ycols) {
    plot(data[,ycol]~data[,xcol], xlab=xcol, ylab=ycol)
  }
}

# Histograms
for(col in cols) {
  hist(data[, col], breaks=20, main=col)
}

# Boxplot:
for (col in cols) {
  boxplot(data[,col], main=col)
}

# Conclustion:
# 1. outliers in X1
# 2. missed values
# 3. errors in X2

# Functions to compare datasets. Useful to estimate effectiveness of cleansing approaches
# Creates boxplots pair by pair comparing mean values
compare.datasets <- function(old.data, new.data, cols) {
  par(mfrow=c(ceiling(length(cols)/2), 2))
  for(col in cols) {
    boxplot(old.data[, col], new.data[, col], main=col, names=c("old", "new"))
    meanD = mean(old.data[, col], na.rm=TRUE)
    meanN = mean(new.data[, col])
    print(paste(meanD, ", new: ", meanN, ", err%: ", (1 - meanD/meanN)*100))
  }
}

# ===========
# Filling missed values

# Rows with missed values:
data[!complete.cases(data),]

# 1. Can omit rows:
newdata <- na.omit(data)
compare.datasets(data, newdata, cols)

# But better:
# 2. Amelia
library(Amelia)
newdata <- amelia(data)
summary(newdata)
plot(newdata)
newdata <- newdata$imputations$imp5
compare.datasets(data, newdata, cols)

# 3. Forward imputation
library(ForImp)
newdata <- data.frame(ForImp(data, p=2))
colnames(newdata) <- names(data)
summary(newdata)
compare.datasets(data, newdata, cols)

# ===========
# Fix outliers

# 1. By mean value
library(outliers)
newdata1 <- rm.outlier(newdata, fill=TRUE, median=FALSE)
compare.datasets(data, newdata1, cols)

# 2. By median value
newdata1 <- rm.outlier(newdata, fill=TRUE, median=TRUE)
compare.datasets(data, newdata1, cols)

# 3. But if there are more outliers then it's not appropriate to fix them,
#    better to remove:
rm.outlier <- function(dataset, col) {
  outlier_tf <- outlier(dataset[, col], logical=TRUE)
  find_outlier <- which(outlier_tf==TRUE, arr.ind=TRUE)
  return(dataset[- find_outlier,])
}
newdata2 <- rm.outlier(newdata1, "X1")
newdata2 <- rm.outlier(newdata2, "X1")
compare.datasets(data, newdata2, cols)

# =========
# Estimate correlation
library(corrgram)

corrgram(newdata2[,cols], order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="")

# Correlated factors:
cor.test(newdata2$X1, newdata2$X2, method=c("pearson")) # reverse dependency

# Regression analysis for correlations:

# Check correctness of model
library(nortest)
library(lmtest)
library(moments)

check.fit <- function(fit, p.value = 0.05) {
  res <- c()
  
  # If p > 0.05 - distribution is normal
  res$NORM_JQ = jarque.test(residuals(fit))$p.value
  res$NORM_PE = pearson.test(residuals(fit))$p.value
  res$NORM_OK = res$NORM_JQ > p.value && res$NORM_PE > p.value
  
  # Autocorrelation doesn't exist if p > 0.05
  res$AC_DW = durbinWatsonTest(fit)$p
  res$AC_BGF1 = bgtest(fit, order=1, type="F")$p.value
  res$AC_BGF2 = bgtest(fit, order=2, type="F")$p.value
  res$AC_BGC1 = bgtest(fit, order=1, type="Chisq")$p.value
  res$AC_BGC2 = bgtest(fit, order=2, type="Chisq")$p.value
  res$AC_OK = res$AC_DW > p.value || 
    (res$AC_BGF1 > p.value && 
       res$AC_BGF2 > p.value && 
       res$AC_BGC1 > p.value && 
       res$AC_BGC2 > p.value)
  
  # Model is homoscedastic if p > 0.05
  res$GS_BP = bptest(fit)$p.value
  res$GS_GQ = gqtest(fit)$p.value
  res$GS_OK = res$GS_BP > p.value && res$GS_GQ > p.value
  
  res$ALL_OK = res$NORM_OK && res$AC_OK && res$GS_OK
  
  # Visualisation:
  #  - residuals plot
  mfrow <- par("mfrow")
  par(mfrow=c(1,3))
  plot(residuals(fit), type="b")
  
  # - histograms with 'normal' line (shows outliers)
  ress <- residuals(fit)
  h <- hist(ress, breaks=20) # histogram of residuals
  xfit<-seq(min(ress),max(ress),length=40) 
  yfit<-dnorm(xfit,mean=mean(ress),sd=sd(ress)) 
  yfit <- yfit*diff(h$mids[1:2])*length(ress) 
  lines(xfit, yfit, col="black", lwd=2)
  
  # - plot of theoretical and real noram distribution
  qqnorm(residuals(fit))
  qqline(residuals(fit))
  par(mfrow=mfrow)
  
  return(res)
}


newdata3 <- newdata2
newdata3$X7 <- factor(newdata3$X7)

fit1 <- lm(Y1~(X1+X2+X7), data=newdata3)
summary(fit1) # X1 correlates with X2

fit1 <- lm(Y1~(X2+X7), data=newdata3)
summary(fit1)
check.fit(fit1)

fit2 <- lm(Y2~(X1+X2+X7), data=newdata3)
summary(fit2)
check.fit(fit2)
