library(rugarch)
library(readxl)
library(rmgarch)

# Loading daily returns
head(r)

## garch specs for currencies
curr_spec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                       variance.model = list(model = "fGARCH", garchOrder=c(1,1), submodel="GJRGARCH"),
                       distribution.model = "norm")

## garch specs for bitcoin
spec_bitcoin = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                          variance.model = list(model = "sGARCH", garchOrder=c(6,0), submodel="GARCH"),
                          distribution.model = "norm")

specs <- multispec(c(curr_spec,spec_bitcoin))

dists <- c('mvnorm', 'mvt')
models = c("DCC", "aDCC", "FDCC")
btc <- r$BTCUSD

ma <- matrix(nrow = length(dists) * length(models), ncol = 7)
colnames(ma) <- c("RUB", "EUR", "JPY", "CNY", "GBP", "CLP", "COP")

`+` <- function(e1, e2) {
  ## unary
  if (missing(e2)) return(e1)
  if (!is.na(suppressWarnings(as.numeric(e1))) & !is.na(suppressWarnings(as.numeric(e2)))) {
    ## both arguments numeric-like but characters
    return(base::`+`(as.numeric(e1), as.numeric(e2)))
  } else if ((is.character(e1) & is.na(suppressWarnings(as.numeric(e1)))) | 
             (is.character(e2) & is.na(suppressWarnings(as.numeric(e2))))) {
    ## at least one true character 
    return(paste0(e1, e2))
  } else {
    ## both numeric
    return(base::`+`(e1, e2))
  }
}

nms <- c(1:6)
for (p in 1:1) {
  for (q in 1:1) {
    for(d in 1:2) {
      for(m in 1:3) {
        nm <- models[m] + "(" + as.character(p) + ","+as.character(q) +") with " + dists[d] 
        print(nm)
        index <- ((p-1)*27) + ((q-1)*9) + ((d-1)*3) + m
        nms[index] <-  nm
      }
    }
  }
}

rownames(ma) <- nms
m.aic    <- ma
m.bic    <- ma
m.LogLik <- ma

for (p in 1:1) {
  for (q in 1:1) {
    for(d in 1:2) {
      for(m in 1:3) {
        dccspecs = dccspec(specs, dccOrder = c(p,q), model = models[m], distribution = dists[d])
        for (i in 1:7) {
          try({
            x <- as.numeric(unlist(r[,i+1]))
            fit <- dccfit(dccspecs, data = data.frame(x,btc))
            
            k <- nrow(fit@mfit$matcoef)
            ll <- fit@mfit$llh
            
            if(!is.null(k) ||  !is.null(ll)) {
              index <- ((p-1)*27) + ((q-1)*9) + ((d-1)*3) + m
              print(colnames(ma)[i] + ' with ' + nms[d*m])
              
              m.aic[index, i] <- (2*k - 2*ll) / (2*1827)
              m.bic[index, i] <- (2*log(k) - 2*ll) / (2*1827)
              m.LogLik[index, i] <- ll
            }
          })
        }
      }
    }
  }
}

res <- list(aic = m.aic, 
            bic = m.bic, 
            loglik = m.LogLik)

rm(curr_spec,dccspecs,ma,spec_bitcoin,specs,btc,d,dists,i,m,index,models,nm,fit,k,ll,nms,p,q,x,m.aic,m.bic,m.LogLik)

