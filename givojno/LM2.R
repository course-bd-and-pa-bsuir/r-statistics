#����������� ���������
library(car)
library(zoo)
library(lmtest)
library(scatterplot3d)
library(nortest)
library(gplots)
library(MASS)
library(ggplot2)
#���������� ������������� ��������� 
s3d <-scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg,pch=16,type="h",highlight.3d=TRUE,col.grid="blue")
m2=lm(mpg~wt+hp,data=mtcars)
m3=lm(mpg~wt+hp+disp,data=mtcars)
s3d$plane3d(m2)
#����� ����������� 
summary(m2)
#���������� ������������� y
fitted(m2)
#�������� ������������� � �� ������������� ���������
coef(m2)
confint(m2)
#������ ������
#�� ��������������
bgtest(m2,order=1,type="F")
durbinWatsonTest(m2)
# ������ �������� ������
residualPlot(m2)
plot(residuals(m2),type="b")
#�������� �� �������������������� ��������
plot(fitted(m2),residuals(m2))
bptest(m2)
ncvTest(fit)
spreadLevelPlot(m2)
#�������� ������������� ��������� ������ ���������
e=residuals(m2)
hist(e,breaks=50,col="lightblue",freq=FALSE)
xfit<-seq(min(e), max(e), length=40)
yfit<-dnorm(xfit, mean=mean(e), sd=sd(e))
lines(xfit, yfit, col="blue", lwd=2)
qqnorm(residuals(m2))
qqline(residuals(m2))
#����� �� ������������
pearson.test(residuals(m2))
spreadLevelPlot(m2)
#������ ������� �������
#����������� ��������
outlierTest(m2)
#������ �������� ��������� (��������������������)
vif(m2)
##
plot(m2)
crPlots(m2)
# ��������� �������
anova(m2,m3) #���� �� ���������� ����� ����������
anova(m2)
#AIC
AIC(m2,m3)
#����� ��������� (��������������)
t.test(mtcars$mpg,mu=20)
#���� ���������� (��������������)
wilcox.test(mtcars$wt,mu=4)
#���������� ���������
t.test(mtcars$mpg~mtcars$vs) #���� ��������� ��� ����������� 
wilcox.test(mtcars$mpg~mtcars$vs)
#�������,�� e�������� ��������� �� �����, ����� ----var.equal = TRUE
t.test(mtcars$cyl,mtcars$am,paired=TRUE)#-#-��� ��������� �������
# ��
stripchart(mtcars$mpg~mtcars$cyl)
boxplot(mtcars$mpg~mtcars$cyl)
plotmeans(mtcars$mpg~mtcars$cyl)

#����������������� ����� ������������� ���������
kruskal.test(mtcars$mpg~mtcars$cyl)
??npmc
install.packages("npmc")
library(npmc)
install.packages("forecast")
library(forecast)
library(zoo)
Birth
bd=Birth
       