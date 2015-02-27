# Use state.x77 and state.region datasets. Combine them, calculate descriptive statistics, 
# draw correlation field, boxplots, dotplots and hists.

rm(list=ls())

states = as.data.frame(state.x77)
states$region = state.region

# Print all statistics
summary(states)

attach(states)
pregion = aggregate(Population, by=list(Region = region), FUN = sum)
mregion = aggregate(Murder, by=list(Region = region), FUN = sum)

# Shows levels for population and murders depending on region
par(mfrow=c(1,2))
plot(pregion, main = 'Population by regions', ylab = 'Population')
plot(mregion, main = 'Murders by regions', ylab = 'Murders')
par(mfrow=c(1,1))

# Shows correlation between murders and illeteracy with regression line
plot(Illiteracy ~ Murder)
abline(lm(Illiteracy~Murder))

# Shows mean number of frost days depending on region
wregion = aggregate(Frost, by=list(Region = region), FUN = mean)
plot(wregion, main = 'Frost days by regions', ylab = 'Frost days')

iregion = aggregate(Income, by=list(Region = region), FUN = mean)
plot(iregion, main = 'Income by regions', ylab = 'Income')

# Boxplot for incomes depending on region
regfact = factor(region)
boxplot(Income ~ region, data = states, varwidth = TRUE)


