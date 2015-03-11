# Построисть регрессию по нескольким параметрам для цены квартиры
data = read.table("kvart.csv", dec=",", sep=";", header=TRUE, fill=FALSE)

# строим модель по 