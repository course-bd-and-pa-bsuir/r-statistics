library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)

s3d = scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, pch = 16, type = 'h')
# linear model of 2 variables
w2 = lm(mpg ~ wt + hp, data = mtcars)
s3d$plane3d(w2)

summary(w2)

residualPlot(w2)
plot(residuals(w2), type = 'b')

# Test for autocorrelation
durbinWatsonTest(w2)

# Another test. First order
bgtest(w2, order = 1, type = 'F')
bgtest(w2, order = 1, type = 'Chisq')
# small autocorrelation

# Second order test
bgtest(w2, order = 2, type = 'F')
bgtest(w2, order = 2, type = 'Chisq')
# no autocorrelation

# Third order test
bgtest(w2, order = 3, type = 'F')
bgtest(w2, order = 3, type = 'Chisq')
# also no autocorrelation

residualPlot(w2)
plot(fitted(w2), residuals(w2))

bptest(w2)
ncvTest(w2)
spreadLevelPlot(w2)

# normality test
res = residuals(w2)
hist(res, breaks = 15)
xfit = seq(min(res), max(res), length = 40)
yfit = dnorm(xfit, mean = mean(res), sd = sd(res))
lines(xfit, yfit, col = 'blue', lwd = 2)

qqnorm(res)
qqline(res)

pearson.test(res)

# test for max regression residual diff and mean of residuals
outlierTest(w2)

plot(w2)
# dependancy of value from all parameters
crPlots(w2)

# фактор инфляции дисперсии
vif(w2)
