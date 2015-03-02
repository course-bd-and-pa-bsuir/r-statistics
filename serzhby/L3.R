setwd('~/code/R/r-statistics/serzhby')
data = read.csv('lec2_source.csv')
data = data$X17.1
# data = rnorm(1000)
library(moments)
library(nortest)
library(vcd)
# test for normality. if p-value > alpha - it's ok
jarque.test(data)

qqnorm(data)
qqline(data)

pearson.test(data)

st = state.x77
# plot(st$Murder, st$Population)

corrgram(st)

corrgram(st, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main = "****")

# проверка значимости коэффициента корреляции
# нулевая гипотеза - не зависит. т.к. p-value < 5% - гипотеза отвергается, значит связаны.
cor.test(st[,2], st[, 4])

att = Arthritis
tab = table(att$Improved)
prop.table(tab)
mytable = xtabs(~Treatment + Improved, data = Arthritis)
mytab = xtabs(~Treatment + Improved + Sex, data = Arthritis)
ftable(prop.table(mytab), c(1, 2))

# check dependency. H0 - no dependancy. (chi-square)
chisq.test(mytable)

# fisher test. 
# fisher.test()

assocstats(mytable)

