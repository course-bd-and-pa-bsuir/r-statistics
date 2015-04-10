prepareData = function() {
  data = read.csv('bzd.csv', header = TRUE, sep = ';')
  s = c('1', '2', '3', '4')
  seasons = c()
  trend = c()
  for(a in seq(1, 7)) {
    seasons = c(seasons, s)
  }
  for(a in 1:17) {
    trend = c(trend, 0)
  }
  for(a in 1:11) {
    trend = c(trend, 1)
  }
  
  data$trend = trend
  data$seasons = seasons
  
  data$ts = ts(data$MINSK, frequency = 4)
  return(data)
}