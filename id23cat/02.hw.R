testing <- function()
{
  stx77 = data.frame(state.x77)
  stx77$Region = state.region
  incomeScale = scale(stx77$Income)
  
  # среднее количество денй в году по региону
  aggregate(stx77$Frost, by=list(Frost=state.region), FUN=mean)
    
  dotchart(stx77$Population, labels=row.names(state.name), cex=.7, main="Число жителей", xlab="количество")
  
  # сортировка по количеству убийств
  x <- x77[order(x77[,5]),]
  
  return (stx77)

}