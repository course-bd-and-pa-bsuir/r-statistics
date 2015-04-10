library(moments)

attach(mtcars)

jarque.test(wt)
qqnorm(wt)
qqline(wt)

cor.test(hp, mpg)
cor(hp, mpg)
plot(hp, mpg)

gearvs = xtabs(~gear + vs)
prop.table(gearvs)
