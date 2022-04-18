library(ConnectednessApproach)

### Load the data for constructng the spllover indexes
spillover_data <- read_excel("data/spilldata.xlsx")

### Format it as zoo since is required to use the ConnectednessApproach package
spillover_data <- zoo(spillover_data[,-1], order.by = spillover_data$Date)
names = c('chile', 'china', 'colombia', 'europe', 'japan', 'russia', 'uk')
head(spillover_data)

for (i in 1:7) {
  dca = ConnectednessApproach(spillover_data[,((i-1)*4+1):((i-1)*4+4)], 
                              model='QVAR', nlag=4, nfore=4)
  print(dca$TABLE)
}


