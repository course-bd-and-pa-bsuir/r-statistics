install.packages(c("astsa", "tseries"))

library(astsa)
library(tseries)
library(nortest)

data(jj) ## Johnson & Johnson data set
help(jj)
time(jj)
cycle(jj)
frequency(jj)
plot(jj, ylab="Earnings per Share", main="J & J") 
