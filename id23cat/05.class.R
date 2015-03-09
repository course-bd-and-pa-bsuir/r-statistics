library(car)
library(zoo)
library(lmtest)
library(nortest)

# построение множественной регрессии
s3d = scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, pch=16, type="h", highlight.3d=T,col.grid="blue")
m2 = lm(mpg~wt+hp, data=mtcars)
s3d$plane3d(m2)

# p-value: 9.109e-12
summary(m2)
#Call:
#  lm(formula = mpg ~ wt + hp, data = mtcars)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.941 -1.600 -0.182  1.050  5.854 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.22727    1.59879  23.285  < 2e-16 ***
#   wt          -3.87783    0.63273  -6.129 1.12e-06 ***
#   hp          -0.03177    0.00903  -3.519  0.00145 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.593 on 29 degrees of freedom
# Multiple R-squared:  0.8268,  Adjusted R-squared:  0.8148 
# F-statistic: 69.21 on 2 and 29 DF,  p-value: 9.109e-12

# Intercept - константа(свободный член ), wt - коэффициент при wt, hp- коэффициент hp
# Estimate - оценка
# Pr(>|t|) - p-value, значение менее доверительного интервала (5%) свидетельствует о наличии зависимости
# Residual standard error - ошибка модели, для сравнения с другими моделями
# Multiple R-squared - доля объяснённой части переменной (в данном случае 99% - очень большое значение)
# Adjusted R-squared полезен при сравнении моделей (выше - лучше)


coef(m2)
confint(m2)
residualPlot(m2)
plot(residuals(m2), type="b")

# проверка автокорреляции
# Darbin-watson test
durbinWatsonTest(m2)

# тест Бреуша-Годфри
# автокорр 1-го порядка имеется
bgtest(m2, order=1, type="F")
bgtest(m2, order=1, type="Chisq")

# автокорреляция порядка 1+2 отсутствует
bgtest(m2, order=2, type="Chisq")

# проверка гетероскедастичности
plot(fitted(m2), residuals(m2))

#бреуша-Пагона
bptest(m2)
# гетеростегастичность отсутствует
# p>0.05 ГС отсутствует
ncvTest(m2)


qqnorm(residuals(m2))
qqline(residuals(m2))
pearson.test(residuals(m2))
spreadLevelPlot(m2)

outlierTest(m2)

plot(m2)

# mfrow ... мого графиков

# зависимость остатков от факторов
crPlots(m2)

# gпроверка мультиколлинеарности
vif(m2)

#

data = read.table("Квартиры5.csv", dec=",", sep=";", header=TRUE, fill=FALSE)