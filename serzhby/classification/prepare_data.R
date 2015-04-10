insertNA = function(dataset) {
  rowCount = length(dataset[, 1])
  columnCount = length(dataset[1, ])
  rows = as.integer(runif(rowCount * 0.1, min = 1, max = rowCount))
  columns = as.integer(runif(length(rows)) * 8 + 1)
  coordsOfNAs = expand.grid(rows, columns)
  coordsOfNAs = data.frame(rows, columns)
  for(i in 1:length(coordsOfNAs[, 1])) {
    dataset[coordsOfNAs[i, 1], coordsOfNAs[i, 2]] = NA
  }
  View(dataset)
  return(dataset)
}

removeNA = function(dataset) {
  return(na.omit(dataset))
  #nas = rep(FALSE, times = length(dataset[,1]))
  #for(i in 1:length(colnames(dataset))) {
  #  nas = nas | is.na(dataset[, i])
  #}
  #nas = !nas
  #filteredSet = dataset[nas, ]
  #return(filteredSet)
}