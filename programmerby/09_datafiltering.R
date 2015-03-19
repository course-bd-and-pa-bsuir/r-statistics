# https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29
# This dataset classifies people described by a set of attributes as good or bad credit risks. 

# 1. Чтение исходных данных
data = read.table("09_german.data.txt", header=FALSE, sep=" ")

# Преобразование данных в читаемый вид
# Переименование столбцов
names(data) = c("ACCOUNT_STATUS", "DURATION", "CREDIT_HISTORY", "PURPOSE", "CREDIT_AMOUNT", "BONDS", "EMPLOYED_YEARS", "INSTALLMENT_RATE", "PERSONAL_STATUS_SEX", "OTHER_D_G", "RESIDENCE", "PROPERTY", "AGE", "PLANS", "HOUSING", "CREDITS_NUM", "JOB", "PEOPLE_LIABLE", "PHONE", "FOREIGN_WORKER", "OUTPUT")

# 1.1 ACCOUNT_STATUS - Статус текущего счета (фактор)
# ZERO        : < 0 DM
# MINIMAL     : 0 <= ... <  200 DM
# RICH        : ... >= 200 DM или регулярное пополнение на протяжении 1-го года
# NO_ACCOUNT  : нету счета
levels(data$ACCOUNT_STATUS) <- c("ZERO", "MINIMAL", "RICH", "NO_ACCOUNT")

# 1.2 DURATION - Срок кредитования (число) в месяцах

# 1.3 CREDIT_HISTORY - Кредитная история (фактор)
# NO_OK     : no credits taken/ all credits paid back duly
# ALL_OK    : all credits at this bank paid back duly
# PAID_OK   : existing credits paid back duly till now
# BAD       : delay in paying off in the past
# VERY_BAD  : critical account/ other credits existing (not at this bank)
levels(data$CREDIT_HISTORY) <- c("NO_OK", "ALL_OK", "PAID_OK", "BAD", "VERY_BAD")

# 1.4 PURPOSE - Цель кредита (фактор)
# CAR_NEW     : car (new)
# CAR_USED    : car (used)
# OTHERS      : others
# FURNITURE   : furniture/equipment
# TV          : radio/television
# DOMESTIC    : domestic appliances
# REPAIRS     : repairs
# EDUCATION   : education
# RETRAINING  : retraining
# BUSINESS    : business
levels(data$PURPOSE) <- c("CAR_NEW", "CAR_USED", "OTHERS", "FURNITURE", "TV", "DOMESTIC", "REPAIRS", "EDUCATION", "RETRAINING", "BUSINESS")

# 1.5 CREDIT_AMOUNT - Размер кредита (число)

# 1.6 BONDS - Статус счета (вклады) (фактор)
# ZERO      :          ... <  100 DM
# MINIMAL   :   100 <= ... <  500 DM
# MEDIUM    :   500 <= ... < 1000 DM
# RICH      :          .. >= 1000 DM
# NONE      :   unknown/ no savings account
levels(data$BONDS) <- c("ZERO", "MINIMAL", "MEDIUM", "RICH", "NONE")

# 1.7 EMPLOYED_YEARS - Опыт работы (фактор)
# UNEMPLOYED : unemployed
# LT_1_YEAR  :       ... < 1 year
# 1_4_YEARS  : 1  <= ... < 4 years  
# 4_7_YEARS  : 4  <= ... < 7 years
# GT_7_YEARS :       .. >= 7 years
levels(data$EMPLOYED_YEARS) <- c("UNEMPLOYED", "LT_1_YEAR", "1_4_YEARS", "4_7_YEARS", "GT_7_YEARS")

# 1.8 INSTALLMENT_RATE - Installment rate in percentage of disposable income (число)
# --- http://money.stackexchange.com/questions/35860/what-is-an-installment-rate-percentage

# 1.9 PERSONAL_STATUS_SEX - Статус + пол (фактор)
# MALE_DIVORCED   : male   : divorced/separated
# FEMALE_MARRIED  : female : divorced/separated/married
# MALE_SINGLE     : male   : single
# MALE_MARRIED    : male   : married/widowed
# FEMALE_SINGLE   : female : single
levels(data$PERSONAL_STATUS_SEX) <- c("MALE_DIVORCED", "FEMALE_MARRIED", "MALE_SINGLE", "MALE_MARRIED", "FEMALE_SINGLE")

# 1.10 OTHER_D_G - Поручители (фактор)
# NONE        : none
# COAPPLICANT : co-applicant
# GUARANTOR   : guarantor
levels(data$OTHER_D_G) <- c("NONE", "COAPPLICANT", "GUARANTOR")

# 1.11 RESIDENCE - Количество лет прожитом на текущем месте жительства? (Present residence since) (число)

# 1.12 PROPERTY - Собственность (фактор)
# REAL_ESTATE : real estate
# SOCIETY     : if not REAL_ESTATE : building society savings agreement/life insurance
# CAR         : if not REAL_ESTATE/A122 : car or other, not in attribute 6
# NONE        : unknown / no property
levels(data$PROPERTY) <- c("REAL_ESTATE", "SOCIETY", "CAR", "NONE")

# 1.13 AGE - Возраст в годах (число)

# 1.14 PLANS - Другие рассрочки (фактор)
# BANK    : bank
# STORES  : stores
# NONE    : none
levels(data$PLANS) <- c("BANK", "STORES", "NONE")

# 1.15 HOUSING - Место жительства (фактор)
# RENT      : rent
# OWN       : own
# FREE      : for free
levels(data$HOUSING) <- c("RENT", "OWN", "FREE")

# 1.16 CREDITS_NUM - Количество кредитов в этом банке (число)

# 1.17 JOB - Место работы (фактор)
# UNEMPLOYED    : unemployed/ unskilled  - non-resident
# UNSKILLED     : unskilled - resident
# EMPLOYED      : skilled employee / official
# TOP_MANAGER   : management/ self-employed/ highly qualified employee/ officer
levels(data$JOB) <- c("UNEMPLOYED", "UNSKILLED", "EMPLOYED", "TOP_MANAGER")

# 1.18 PEOPLE_LIABLE - Количество людей, которым ... ? (число)
# Number of people being liable to provide maintenance for

# 1.19 PHONE - Наличие телефона - данные от 1994 года (фактор)
# NO    : none
# YES   : yes, registered under the customers name
levels(data$PHONE) <- c("NO", "YES")

# 1.20 FOREIGN_WORKER - Иностранец (фактор)
# NO  : no
# YES : yes
levels(data$FOREIGN_WORKER) <- c("NO", "YES")

# 1.21 OUTPUT - Выходное значение. Можно ли дать кредит или нет?
data$OUTPUT = data$OUTPUT - 1

# source_data - исходные данные
# data в будущем - данные с пропущенными значениями
source_data = data

# 2. Добавление 10% пропущенных данных в случайных строках и столбцах (кроме столбца OUTPUT)
set.seed(1) # (!!!)
random_y = sample(length(source_data[,1]), length(source_data[,1])*0.1)
max_x = length(source_data)-1
data = source_data
data$CORRUPTED = 0
for (i in 1:length(random_y)) {
  data[random_y[i], sample(length(data)-1, 1)] = NA 
  data$CORRUPTED[random_y[i]] = 1
}

# 3. Проверка данных
library(mi)
mi.info(data)

##
##
##
##
## ПЕСОЧНИЦА
## Попытка использовать разные методы для восстановления данных
##
##
##
##

## Данные о возрасте, объеме кредита и другие должны быть положительными и адекватными
boxplot(data$AGE)
boxplot(data$CREDITS_NUM)
boxplot(data$DURATION)
# ........
# Всё норме!

# Попытка использовать магические библиотеки mi, mice
library(mice)

# m = mice(rawm2)

# status, duration, credit history, credit amount, savings, housing and foreign worker.
# STATUS, DURATION, CREDIT_HISTORY, PURPOSE, CREDIT_AMOUNT, BONDS, EMPLOYED_YEARS, INSTALLMENT_RATE, STATUS_SEX, OTHER_D_G, RESIDENCE, PROPERTY, AGE, PLANS, HOUSING, CREDITS_NUM, JOB, PEOPLE_LIABLE, PHONE, FOREIGN_WORKER, OUTPUT
#mice_imp = mice(raw, method=c("", "", "", "polyreg", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), m=10)
#data = complete(mice_imp)
#data$PURPOSE[which(is.na(raw$PURPOSE))]
#source_raw$PURPOSE[which(is.na(raw$PURPOSE))]

mice_imp = mice(data)
fixed = complete(mice_imp)

library(MASS) # stepAIC
library(nortest) # pearson.test
library(VIM) # kNN
library(e1071) # impute -- херня
library(logistf) # logistf - логистическая регрессия
library(forecast) # accuracy

for (i in 1:100000) {
  m = lm(log(AGE) ~ log(DURATION) + PROPERTY + PHONE, data = data[sample(length(data[,1]), 200),], na.action=na.exclude); #summary(m); vif(m); cat("\n"); i = which(is.na(data$AGE)); exp(predict(m, newdata = data[i, ])); cat("\n"); source_data$AGE[i]
  sm = summary(m)
  j = which(is.na(data$AGE))
  
  tryCatch({
    a = accuracy(exp(predict(m, newdata = data[j, ])), source_data$AGE[j])
    #if (sm$r.squared > 0.7) {
    if (a[5] < 13 && sm$r.squared > 0.5) {
      cat("YES\n")
      break
    }
  }, warning = function(ex) {
  }, error = function(ex) {
  }, finally={
  })
}

j = which(is.na(data$AGE))
accuracy(fixed$AGE[j], source_data$AGE[j])

knn_imp = kNN(raw, variable = c("PROPERTY")); i = which(is.na(raw$PROPERTY)); knn_imp$PROPERTY[i]; source_raw$PROPERTY[i]

knn_imp = kNN(raw, variable = c("AGE")); knn_imp$AGE[i]; source_raw$AGE[i]

# Поля, для которых можно взять среднее значение
# AGE - возраст
# CREDITS_NUM - количество кредитов
# DURATION - срок кредита
fixed$AGE[is.na(data$AGE)] = as.integer(mean(data$AGE, na.rm=TRUE))

data$CREDITS_NUM[is.na(data$CREDITS_NUM)] = as.integer(mean(data$CREDITS_NUM, na.rm=TRUE))

# 3. Добавление новых столбцов
fixed$SEX = 0
fixed$SEX[which(fixed$PERSONAL_STATUS_SEX == "FEMALE_MARRIED")] = 1
fixed$SEX[which(fixed$PERSONAL_STATUS_SEX == "FEMALE_SINGLE")] = 1

fixed$CREDIT_PER_MONTH = fixed$CREDIT_AMOUNT / fixed$DURATION

fixed$CREDIT_HISTORY_NO_OK = 0
fixed$CREDIT_HISTORY_NO_OK[which(fixed$CREDIT_HISTORY == "NO_OK")] = 1
fixed$CREDIT_HISTORY_ALL_OK = 0
fixed$CREDIT_HISTORY_ALL_OK[which(fixed$CREDIT_HISTORY == "ALL_OK")] = 1
fixed$CREDIT_HISTORY_PAID_OK = 0
fixed$CREDIT_HISTORY_PAID_OK[which(fixed$CREDIT_HISTORY == "PAID_OK")] = 1
fixed$CREDIT_HISTORY_BAD = 0
fixed$CREDIT_HISTORY_BAD[which(fixed$CREDIT_HISTORY == "BAD")] = 1
fixed$CREDIT_HISTORY_VERY_BAD = 0
fixed$CREDIT_HISTORY_VERY_BAD[which(fixed$CREDIT_HISTORY == "VERY_BAD")] = 1

fixed2 = subset(fixed, select = -c(CREDIT_HISTORY) )

status, duration, credit history, credit amount, savings, housing and foreign worker.

m = lm(OUTPUT ~ ACCOUNT_STATUS + DURATION + CREDIT_HISTORY + PURPOSE + CREDIT_AMOUNT + BONDS + EMPLOYED_YEARS + INSTALLMENT_RATE + PERSONAL_STATUS_SEX + OTHER_D_G + RESIDENCE + PROPERTY + AGE + PLANS + HOUSING + CREDITS_NUM + JOB + PEOPLE_LIABLE + PHONE + FOREIGN_WORKER, data = fixed[sample(length(fixed[,1]), 200),], na.action=na.exclude); summary(m); vif(m);
sm = summary(m)
