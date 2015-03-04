fit = lm(women$weight ~ women$height, data = women)
summary(fit)
fitted(fit)

plot(residuals(fit), type="b")
plot(women$height, women$weight)

fit2=lm(women$weight ~ women$height +I(women$height^2))

plot(women$height, women$weight)
lines(women$height, fitted(fit2))
summary(fit2)

library(car)
scatterplot(weight ~ height, data=women)
scatterplotMatrix(mtcars)

coef(fit2)
confint(fit2)

# строим гистограмму остаткоы
hist(residuals(fit2), breaks=10)
#проверк остатков на нормальность, строим на нормальной вероятностной бумаге
qqnorm(residuals(fit2))
qqline(residuals(fit2))
qqPlot(fit2)

pearson.test(residuals(fit2))