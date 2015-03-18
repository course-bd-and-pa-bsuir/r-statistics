testModel = function(model, verbose = FALSE) {
  print(summary(model))
  res = residuals(model)
  
  if(testLjungBox(res, verbose)) {
    cat("Box-Ljung: no autocorrelation in residuals")
  }
  else {
    cat("Box-Ljung: autocorrelation in residuals")
  }
  cat("\n")
  
  if(testPearson(res, verbose)) {
    cat("Pearson: Normal distribution")
  }
  else {
    cat("Pearson: Non-normal distribution")
  }
  cat("\n")
  
  if(testBreuschPagan(model, verbose)) {
    cat("Breusch-Pagan: No heterosсedasticity")
  }
  else {
    cat("Breusch-Pagan: Heterosсedasticity in residuals")
  }
  cat("\n")
}

testLjungBox = function(residuals, verbose, order = 15) {
  noCorrelation = TRUE
  if(verbose) {
    cat("Ljung-Box test\n")
  }
  for (i in 1:order) {
    bt = Box.test(residuals, lag = i, type = "Ljung-Box")
    # print(c(bt$statistic, bt$p.value))
    if(verbose) {
      cat("Lag: ", i, " statistic = ", bt$statistic, " p-value = ", bt$p.value, "\n")
    }
    if(bt$p.value < 0.05) {
      cat("Autocorrelation of order ", i, " p-value = ", bt$p.value, "\n")
      noCorrelation = FALSE
    }
  }
  return(noCorrelation)
}

testPearson = function(residuals, verbose) {
  library(nortest)
  pt = pearson.test(residuals)
  if(verbose) {
    cat("Pearson statistic = ", pt$statistic, " p-value", pt$p.value, "\n")
  }
  return(pt$p.value > 0.05)
}

testBreuschPagan = function(model, verbose) {
  library(lmtest)
  bp = bptest(model)
  if(verbose) {
    cat("Breusch-Pagan statistic = ", bp$statistic, " p-value = ", bp$p.value, "\n")
  }
  return(bp$p.value > 0.05)
}
