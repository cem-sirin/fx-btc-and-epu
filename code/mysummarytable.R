mysummarytable <- function(x) {
  sum_stats <- matrix(0, nrow=length(x), ncol=10)
  for (i in 1:length(r[,-1])) {
    sum_stats[i, 1] <- mean(as.matrix(x[,i]))
    sum_stats[i, 2] <- sd(as.matrix(x[,i]))
    sum_stats[i, 3] <- min(as.matrix(x[,i]))
    sum_stats[i, 4] <- max(as.matrix(x[,i]))
    sum_stats[i, 5] <- kurtosis(as.matrix(x[,i]))+3
    sum_stats[i, 6] <- skewness(as.matrix(x[,i]))
    sum_stats[i, 7] <- jarque.bera.test(as.matrix(as.matrix(x[,i])))$statistic
    sum_stats[i, 8] <- adf.test(as.matrix(as.matrix(x[,i])))$statistic
    sum_stats[i, 9] <- Box.test(as.matrix(x[,i]), lag = 1, type = "Ljung")$statistic
    sum_stats[i, 10] <- cor.test(x=as.matrix(as.matrix(x[,i])), y=x$BTCUSD, method = 'spearman', exact=FALSE)$statistic
    
    row.names(sum_stats) <- colnames(x)
    colnames(sum_stats) <- c('Mean', 'SD', 'Min', 'Max', 'Kurt', 'Skew', 'JB', 'ADF', 'Ljung-Box', 'Spearman')
  }
  return(sum_stats)
}