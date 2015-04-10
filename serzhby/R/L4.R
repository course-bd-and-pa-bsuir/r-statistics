fit = lm(weight ~ height, data = women)
summary(fit)

# modeled
fitted(fit)

# residuals
residuals(fit)

# check type of dependancy
plot(women$height, women$weight)
abline(fit)

# better fit. decide by F-statistics
fit2 = lm(weight ~ height + I(height^2), data = women)
summary(fit2)
plot(women$height, women$weight)
lines(women$height, fitted(fit2))


library(car)
scatterplot(weight ~ height, data = women)
scatterplotMatrix(mtcars)

# regression of mpg depending on wt
scatterplot(mpg ~ wt, data = mtcars)
carsfit = lm(mpg ~ wt + hp, data = mtcars)
summary(carsfit)
# show coeffs only
coef(carsfit)
# confidence interval
confint(carsfit)

# check residuals for normal distribution
hist(residuals(fit2), breaks = 2)
qqnorm(residuals(fit2))
qqline(residuals(fit2))
qqPlot(carsfit)
library(moments)
jarque.test(residuals(carsfit))

setwd('~/code/R/BDPA//r-statistics/serzhby/')
data = read.csv('l3_source.csv', sep = ';')
colnames(data) = c('id', 'y', 'x1', 'x2', 'x3', 'x4')
attach(data)
#View(data)
#data = log(data)
fit = lm(y ~ x4, data = data)
summary(fit)
plot(y ~ x1)
abline(lm(y ~ x1))
