new = read.csv("/Users/user/Downloads/lec2_source.csv")

library(moments)
jarque.test(new$X17.1);

qqnorm(new$X17.1)
qqline(new$X17.1)

library(nortest)
pearson.test(new$X17.1)

st = state.x77
cov(st);
cor(st);

cor(st[,2:5])

library(corrgram)
corrgram(st)
corrgram(st,order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.pie,text.panel=panel.txt,main="*****")

