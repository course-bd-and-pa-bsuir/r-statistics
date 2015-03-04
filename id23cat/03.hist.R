")
read.table(header = TRUE, text = "
a b
1 2
3 4
")
x <- c(0:10, 50)
x
xm <- mean(x)
c(xm,mean(x,trim=0.10))
mean(USArrests)
mean(USArrests, 0.2)
mean(USArrests, trim=0.2)
mean(USArrests, trim = 0.2)
USArrests
mean(USArrests, trim = 0.2)
x=3; y<-5; x+y
x = c(1,2,3, 7); y= c(2,3,5)
x+y
x = c(1,2,3, 7); y= c(2,3)
x+y
x*y
x = c(1,2,3, 7); y= c(2,3,4,5)
x*y
x %*% y
y
t(y)
ty = t(y)
x * ty
x %*% ty
x = seq(1,10)
x = c(1,2,3, 7); y= c(2,3,4,5)
x1 = seq(1,10)
y1=2:11
x1+y1
x2=seq(1,10,by=2)
y2=seq(1,10,length=2)
rep(c(5,8), 3)
rep(c(5,8), 2)
length(x2)
x3=c(1:10)
maen(x)
maen(x3)
mean(x3)
min(x3)
median(x3)
mean(x), min(x), median(x), max(x), var(x)
c(mean(x), min(x), median(x), max(x), var(x))
c(mean(x3), min(x3), median(x3), max(x3), var(x3))
x3[3]
x3[1:3]
x3[4:3]
x3[7:3]
x3[-(1:6)]
e = list(thing="hat", size="8.25")
e
e = list(thing="hat", size="8.25", tt="11")
e
e$tt
e$size
phone = factor(c('iphone', 'htc', 'iphone', 'samsung',
'iphone',
'samsung'))
levels(phone)
min(phone)
as.integer(phone)
factor(phone)
a <- array(c(1,2,3,4,5,6,7,8,9,10,11,12),dim=c(3,4)))
a <- array(c(1,2,3,4,5,6,7,8,9,10,11,12),dim=c(3,4))
View(a)
View(a)
View(a)
View(ty)
typeof(a)
typeof(phone)
x = c(1,2,3); y = c(4,5,6)
rbind(x,y)
cbind(x,y)
dim(cbind(x,y))
matrix(c(1,2,3,4,5,6),nr=3);
matrix(c(1,2,3,4,5,6),nrow=3, ,byrow=T)
x<-matrix(c(1,2,3,4),nr=2)
x
y<-matrix(c(5,6),nr=2)
y
x%
*%y
x %*% y
x * y
solve(matrix(c(1,2,3,4),nr=2))
df = data.frame(a=c(1,2,3,4,5),b=c(2,3,4,5,6));df
df.a
df$a
df$b
`%myop%` <- function(a, b) {2*a + 2*b}; 1 %myop% 1
x4=edit(x3)
x4
x5=fix(x3)
head(x3)
tail(x3)
tail(x3,3)
head(x3,3)
setClass("Student", representation(name = "character",
score="numeric"))
studenta = new ("Student", name="david", score=80 )
studentb = new ("Student", name="andy", score=90 )
setMethod("show", signature("Student"),
function(object) {
cat(object@score+100)
})
studenta.show()
setGeneric("getscore", function(object)
standardGeneric("getscore"))
m1 = matrix(1,2,3,4,5,6,7,8,9)
m1 = matrix(1,2,3,4,5,6,7,8,9, nr=3)
m1 = matrix(c(1,2,3,4,5,6,7,8,9), nr=3)
as.matrix(1:9)
m1
m2 = t(matrix(c(1,2,3,4,5,6,7,8,9), nr=3))
m2
m1 * m2
m2 = t(matrix(c(9:1), nr=3))
m2
m1 * m2
m2 = matrix(c(9:1), nr=3)
m1 * m2
m1
m2
m2 = matrix(c(1:9), nr=3)
m2
m1 * m2
m1
m2
m1 %*% m2
setClass("Student", representation(name = "character",
score="numeric"))
studenta%name
setMethod("show", signature("Student"),
function(object) {
cat(object@score+100)
})
setMethod("show", signature("Student"),
function(object) {
cat(object@score+100)
})
studenta.show
studenta.show()
studenta
studenta@show()
studenta@name
studenta@score
studenta@show()
library(rpart)
CRAN
install
library()
library(parallel)
library(ggplot2)
library(help=ggplot2)
install.packages("ggplot2")
library(help=ggplot2)
library(arm)
library("arm")
install.packages("arm"
)
install.packages("ggplot2")
install.packages("glmnet")
install.packages("igraph")
install.packages("lme4")
install.packages("lubridate")
install.packages("RCurl")
install.packages("RCurl")
install.packages("reshape")
install.packages("RJSONIO")
install.packages("lm")
install.packages("tm")
install.packages("XML")
install.packages("e1071")
setwd("~/Work/BigDataCourse")
install.packages("permute")
mur = state.x77
state.division
state.x77
x77 = state.x77
x77$1
View(`x77`)
View(`x77`)
x77$Income
x77@Income
x77.Income
cars
cars=mtcars
x77[1]
x77[1,]
x77[,1]
order(x77[,1])
order(x77[,2])
x77[,2]
stat$state.region
state.region
order(state.region)
reg = state.region
sreg[order(reg)]
reg[order(reg)]
x77
x77[order(x77[1])]
summary(x77)
summary(x77[1])
summary(x77[,1])
x77[order(x77[,1])]
View(`x77`)
View(`x77`)
x77
x77(order(x77[,1])]
x77(order(x77[,1])
)
x77(order(x77[,1]))
x77[order(x77[,1])]
x77
x77[order(-x77[,1])]
x77[order(x77[,1])]
x77[order(x77[,1]),]
x77[max(x77[,1]),]
max(x77[,1])
order(x77[,1])
su = summary(x77)
su
sapply(x77[,c(1,5)])
sapply(x77[,c(1,5)], sd)
x77[,c(1,5)]
apply(mtcars[,c(3,4)],sd)
sapply(mtcars[,c(3,4)],sd)
sapply(x77[,c(1,5)], sd)
mtcars[,c(3,4)]
sapply(x77[,c(1,5)], mean)
sapply(x77[,c(1,5)], sd)
sapply(x77[,c(1,5)], max)
t1 = x77[,c(1,5)]
t2 = mtcars[,c(3,4)]
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
t1 = list(x77[,1], x77[,2])
sapply(t1, sd)
sapply(x77[,c(1,5)], sd)
lapply(x77[,c(1,5)], sd)
vapply(x77[,c(1,5)], sd)
t1 = list(Population=x77[,1], Murder=x77[,2])
t1$Population
t1$Murder
setwd("~/Work/BigDataCourse")
state
state
list(Population=x77[,1], Murder=x77[,2])
list(x77)
list(x77).Area
t = list(x77)
t$Area
as.list(x77)
list(x = cars[,1], y = cars[,2])
cars
x77
scale(x77[,1])
source("02.hw.R")
testing()
testing()
source("02.hw.R")
source("02.hw.R")
testing()
source("02.hw.R")
testing()
fivenum(x77)
stx77 = list(Population=t[,1], Income=t[,2], Illiteracy=t[,3],
LifeExp=t[,4], Murder=t[,5], HSGrad=t[,6], Frost=t[,7], Area=t[,8])
stx77 = list(Population=t[,1], Income=t[,2], Illiteracy=t[,3],
LifeExp=t[,4], Murder=t[,5], HSGrad=t[,6], Frost=t[,7], Area=t[,8])
stx77 = list(Population=t[,1], Income=t[,2], Illiteracy=t[,3], LifeExp=t[,4], Murder=t[,5], HSGrad=t[,6], Frost=t[,7],Area=t[,8])
stx77 = list(Population=t[,1], Income=t[,2], Illiteracy=t[,3], LifeExp=t[,4], Murder=t[,5], HSGrad=t[,6], Frost=t[,7],Area=t[,8])
t=x77
stx77 = list(Population=t[,1], Income=t[,2], Illiteracy=t[,3], LifeExp=t[,4], Murder=t[,5], HSGrad=t[,6], Frost=t[,7],Area=t[,8])
stx77
fivenum(x77[1])
fivenum(stx77$Population)
library(pastecs)
stat.desc(mtcars[vars])
stat.desc(mtcars[1])
fivenum(mtcars$mpg)
describe(mtcars[,c(3,4)])
describe(mtcars[,c(3,4)]) -- psych
describeBy(mtcars[,c(3,4)])
describeBy(mtcars[,c(3,4)],list(cyl=mtcars$cyl))
library(psych)
describeBy(mtcars[,c(3,4)])
describeBy(x77[,c(3,4)])
describeBy(mtcars[,c(3,4)],list(cyl=mtcars$cyl))
describeBy(mtcars[,c(3,4)],list(cyl=mtcars$cyl,am=mtcars$am))
aggregate(mtcars[vars], by=list(am=mtcars$am), mean)
aggregate(car[c("mpg","disp")], by=list(am=mtcars$am), mean)
aggregate(cars[c("mpg","disp")], by=list(am=mtcars$am), mean)
aggregate(x77[,1], mean)
aggregate(x77[,1], FUN=mean)
aggregate(as.data.frame(x77[,1]), FUN=mean)
aggregate(stx77$Frost, by=state.region, FUN=mean)
aggregate(stx77$Frost, by=as.data.frame(state.region), FUN=mean)
state.region
aggregate(stx77$Frost, by=list(Frost=state.region), FUN=mean)
aggregate(Frost=stx77$Frost, by=list(Region=state.region), FUN=mean)
aggregate(stx77$Frost, by=list(Region=state.region), FUN=mean)
describe.by(mtcars[vars], mtcars$am)
describeBy(car, list(car$am,car$gear))
dstats <- function(x)(c(mean=mean(x), sd=sd(x)))
by(mtcars[vars], mtcars$am, dstats)
by(mtcars[mpg], mtcars$am, dstats)
by(mtcars$mpg, mtcars$am, dstats)
dotchart(x77[1], labels=row.names(state.name), cex=.7)
dotchart(x77[,1], labels=row.names(state.name), cex=.7)
dotchart(stx77$Population, labels=row.names(state.name), cex=.7, Main="Число жителей", xlab="количество")
dotchart(stx77$Population, labels=row.names(state.name), cex=.7, ьain="Число жителей", xlab="количество")
dotchart(stx77$Population, labels=row.names(state.name), cex=.7, main="Число жителей", xlab="количество")
dotchart(stx77$Population, labels=row.names(state.name), cex=.7, main="Число жителей", xlab="количество")
x <- mtcars[order(stx77$Murder),]
x
x <- stx77[order(stx77$Murder),]
View(cars)
View(cars)
x <- x77[order(x77[,5]),]
x
x <- x77[order(x77[,5]),]
x[0]
x[0,1]
x[0,2]
x[1,0]
x[0,]
x[,0]
x[3] <- factor(x[3])
x
x[3] <- factor(x[3])
x[3] <- factor(state.region[3])
x[3] <- factor(state.region[1])
x
x <-c(state.x77)
x[3]<-factor(state.region)
x[,3]<-factor(state.region)
state.region[1]
state.region[,1]
state.region
state.region[]
x[,3]<-factor(state.region[])
x[]<-factor(state.region[])
stx77<=factor(vector(state.region))
stx77<=factor(vector(state.region[1]))
stx77<=factor(vector(state.region[,1]))
state.region[,1]
state.region
state.region[1]
state.region[2]
state.region[3]
state.region[4]
vector(state.region)
stx77$Region = state.region
stx77
stx77[1]
stx77[,1]
stx77$Region
View(cars)
typeof(cars)
typeof(stx77)
stx77
state.x77
data.frame(state.x77)
tt = data.frame(state.x77)
stx77 = data.frame(state.x77)
View(stx77)
stx77$Region = state.region
View(stx77)
View(stx77)
x = stx77[order(stx77$Murder)]
x = stx77[order(stx77$Murder),]
x$Illiteracy = factor(x$Illiteracy)
x$Illiteracy = factor(x$Region)
factor(x$Region)
x$Region
factor(x$Illiteracy, levels=)
max(stx77$Illiteracy)
min(stx77$Illiteracy)
factor(stx77$Illiteracy)
factor(stx77$Illiteracy,levels=c(0.5:3))
c(0.5:3)
c(0.5:0.5:3)
seq(0.5,3,0.5)
x
x$color[x$Region=="Northeast"] = "red"
x$color[x$Region=="South"] = "yellow"
x$color[x$Region=="North Central"] = "black"
x$color[x$Region=="West"] = "blue"
dotchart(x$Murder, labels=row.nmaes(x),cex=.7,groups=x$Region,gcolor="green", color=x$color, pch=19)
dotchart(x$Murder, labels=row.names(x),cex=.7,groups=x$Region,gcolor="green", color=x$color, pch=19)
install.packages("fBasics")
install.packages("gss")
install.packages("nortest")
install.packages("quadprog")
install.packages("stabledist")
install.packages("timeDate")
install.packages("timeSeries")
install.packages("tseries")
library(moments)
install.packages("moments")
library(moments)
read.table("data.csv")
data = read.table("data.csv")
data = read.table("data.csv")
data
View(data)
View(data)
jarque.test(data)
install.packages("zoo")
read.table("data.csv")
jarque.test(data)
jarque.test(as.vector(data))
jarque.test(data[,1])
jarque.test(data[1])
data = read.table("data.csv", dec=".")
data = read.table("data.csv", dec=",")
View(data)
View(data)
jarque.test(data[.1])
jarque.test(data[,1])
?jarque.test
library(moments)
jarque.test(data[,1])
data = read.table("data.csv", dec=",")
jarque.test(data[,1])
qqnorm(data)
qqnorm(data[,1])
qqline(data[,1])
library(nortest)
pearson.test(data[,1])
hist(data[,1])
hist(data[.1], col="red", breaks=12)
hist(data[,1], col="red", breaks=12)
hist(data[,1], col="red", breaks=30)
hist(data[,1], col="red", breaks=50)
hist(data[,1], col="red", breaks=`0)
hist(data[,1], col="red", breaks=`0)
hist(data[,1], col="red", breaks=10)
stx=state.x77
cov(stx)
cor(stx)
cor(stx)
cor(stx)
cor(stx,method="spearman")
library(corra)
library(corrgram)
install.packages("corrgram")
library(corrgram)
corrgram(stx)
corrgram(stx,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,text.panel=panel.txt)
cor.test(stx[,2], stx[,3])
View(stx)
View(stx)
cor.test(stx[,2], stx[,4])
cor.test(stx[,8], stx[,4])
library(vcd)
install.packages("vcd")
library(vcd)
Arthritis
art=Arthritis
table(art)
table(art$Improved)
tab=table(art$Improved)
prop.table(tab)
mytab=xtabs(art$Treatment + art$Improved, data=art)
mytab=xtabs(~art$Treatment + art$Improved, data=art)
mytab
mytab=xtabs(~art$Treatment + art$Improved + art$Sex, data=art)
xtabs(~art$Treatment + art$Improved + art$Sex, data=art)
mytab=xtabs(~art$Treatment + art$Improved + art$Sex, data=art)
ftable(mytab)
addmargins(mytab)
mytab
mytable=xtabs(art$Treatment + art$Improved, data=art)
mytable=xtabs(~art$Treatment + art$Improved, data=art)
chisq.test(mytable)
mytable
fisher(mytable)
fisher.test(mytable)
fisher.test(mytab)
fisher.test()
assocstats(mytable)
car=mtcars
car
car
rm(cars)
cor(car)
car
View(car)
View(car)
car[,c(1,2)]
car[,c(1,3,4,5,6,7)]
cor(car[,c(1,2,3,4,5,6,7)])
corrgram(car[,c(1,2,3,4,5,6,7)])
cor.test(car$,pg, car$cyl)
cor.test(car$mpg, car$cyl)
cor.test(car$qsec, car$drat)
cor.test(car$qsec, car$wt)
cor.test(car$qsec, car$hp)
table(~car$qsec, car$hp)
table(~car$qsec+ car$hp, data=car)
table(~car$qsec + car$hp, data=car)
table(~car$qsec)
table(car$qsec)
prop.table(table(car$qsec))
xtabs(~car$qsec + car$hp, data=car)
savehistory("~/Work/BigDataCourse/03.hist.R")
