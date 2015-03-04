data = read.table("data.csv",dec=",")

library(moments)
# Статистика Жака-Бера
JB = jarque.test(data[,1])
print(JB)

qqnorm(data[,1])
qqline(data[,1])

library(nortest)
# Тест Пирсона на нормальность распределения
PEAR=pearson.test(data[,1])
print(PEAR)


stx = state.x77

#print(cor(stx,method="spearman"))

library(corrgram)
corrgram(stx)
corrgram(stx,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,text.panel=panel.txt)

# Проверка значимости коэффициента корреляции
CORT = cor.test(stx[,2], stx[,3])
print(CORT)

CORT1 = cor.test(stx[,2], stx[,4])
print(CORT1)

# p-val > 0.05 --> H0 - истинна (корреляция =(->) 0)
CORT2 = cor.test(stx[,4], stx[,8])
print(CORT2)

library(grid)
art = Arthritis
# таблицы сопряженности
tab = table(Arthritis$Improved)
print(tab)
ptab = prop.table(tab)
print(ptab)
mytab = xtabs(~art$Treatment + art$Improved, data=art)
print(mytab)
mytable = xtabs(~art$Treatment + art$Improved + art$Sex, data=art)
print(mytable)
ftable(mytable)
addmargins(mytable)

print(chisq.test(mytable))
print(fisher.test(mytab))

# меры связи
assocstats(mytab)