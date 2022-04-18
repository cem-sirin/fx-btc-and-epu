library(dccmidas)
library(rumidas)


epu <- read_excel('data/log_diff_epu.xlsx') #log returns in epu
ucry <- read_excel('data/UCRY_log_diff.xlsx')

# converting returns an xts (extensible time series) object
rts <- xts(r[,-1], order.by = as.POSIXct(r$DATE))
epu <- xts(epu[,-1], order.by = as.POSIXct(epu$Date))
ucry <- xts(ucry[,-1], order.by = as.POSIXct(ucry$Date))
colnames(ucry) <- c("Policy", "Price")
head(ucry)

# estimation parameters
K_c=12## number of lagged standardized residuals to estimate the long run correlation
N_c=24## number of lagged residuals to estimate the lagged standardized residuals

### DCC-Midas as EPU of the US as the external regressor
for (i in 1:7) {
  print(colnames(rts)[i])
  li <- list(rts[,i],rts$BTCUSD)
  mv <- mv_into_mat(rts[,i], epu$US, K=12, type = "monthly")
  MV <- list(mv, mv)
  
  dccmidas_est <- dcc_fit(li, univ_model="DAGM_noskew", distribution="std",
                          MV = MV, K=12, corr_model="DCCMIDAS", K_c=K_c, N_c=N_c)
  summary.dccmidas(dccmidas_est)
  corr = dccmidas_est[["R_t"]][1,2,]
  plot(x=r$DATE, y=corr, type ='l')
  plot_dccmidas( dccmidas_est,
                 K_c = K_c,
                 vol_col = "black", long_run_col = "red", cex_axis = 0.5,
                 LWD = 2)
}
### DCC-Midas as UCRY as the external regressor
for (i in 1:7) {
  print(colnames(rts)[i])
  li <- list(rts[,i],rts$BTCUSD)
  mv <- mv_into_mat(rts[,i], ucry$Price, K=12, type = "monthly")
  MV <- list(mv, mv)
  
  dccmidas_est <- dcc_fit(li, univ_model="DAGM_noskew", distribution="std",
                          MV = MV, K=12, corr_model="DCCMIDAS", K_c=K_c, N_c=N_c)
  summary.dccmidas(dccmidas_est)
  corr = dccmidas_est[["R_t"]][1,2,]
  plot(x=r$DATE, y=corr, type ='l')
  plot_dccmidas( dccmidas_est,
                 K_c = K_c,
                 vol_col = "black", long_run_col = "red", cex_axis = 0.5,
                 LWD = 2)
}
### DCC-Midas as EPU of the US as the external regressor
epu2 <- cbind(epu$Russia,epu$Europe,epu$Japan, epu$China,epu$UK, epu$Chile,epu$Colombia)
epu2 <- xts(x=epu2, order.by = epu$Date)
for (i in 1:7) {
  print(colnames(rts)[i])
  li <- list(rts[,i],rts$BTCUSD)
  mv <- mv_into_mat(rts[,i], epu2[,i], K=12, type = "monthly")
  MV <- list(mv, mv)
  
  dccmidas_est <- dcc_fit(li, univ_model="DAGM_noskew", distribution="std",
                          MV = MV, K=12, corr_model="DCCMIDAS", K_c=K_c, N_c=N_c)
  summary.dccmidas(dccmidas_est)
  corr = dccmidas_est[["R_t"]][1,2,]
  plot(x=r$DATE, y=corr, type ='l')
  plot_dccmidas( dccmidas_est,
                 K_c = K_c,
                 vol_col = "black", long_run_col = "red", cex_axis = 0.5,
                 LWD = 2)
}
corr
