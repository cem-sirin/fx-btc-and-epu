

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

my_dcc_selector <- function(vars, p_max, q_max) {
  btc <- vars$BTCUSD
  p_max <- 1
  q_max <- 1
  m_max <- 3 # number of model types
  ncols <- (p_max)*(q_max)* m_max # number of model combinations
  m <- matrix(nrow = ncol(vars), ncol = ncols)
  rownames(m) <- colnames(vars)
  
  m.aic    <- m
  m.bic    <- m
  m.LogLik <- m
  
  ## Declaration of specs
  ## garch specs for currencies
  curr_spec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                         variance.model = list(model = "fGARCH", garchOrder=c(1,1), submodel="GJRGARCH"),
                         distribution.model = "norm")
  
  ## garch specs for bitcoin
  spec_bitcoin = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                            variance.model = list(model = "sGARCH", garchOrder=c(6,0), submodel="GARCH"),
                            distribution.model = "norm")
  
  spec_dcc  <- dccspec(uspec=multispec(c(curr_spec, spec_bitcoin)), VAR=FALSE, dccOrder=c(1,1), model="DCC", distribution = "mvnorm")
  spec_adcc <- dccspec(uspec=multispec(c(curr_spec, spec_bitcoin)), VAR=FALSE, dccOrder=c(1,1), model="aDCC", distribution = "mvnorm")
  spec_fdcc <-  dccspec(uspec=multispec(c(curr_spec, spec_bitcoin)), VAR=FALSE, dccOrder=c(1,1), model="FDCC",groups=c(1,2), distribution = "mvnorm")
  
  specs <- c(spec_dcc, spec_adcc, spec_fdcc)
  
  rm(curr_spec, spec_bitcoin, spec_dcc, spec_adcc, spec_fdcc)
  

  for(d in 1:1) {
    for(m in 1:3) {
      dccspecs = specs[m]
      for (i in 1:7) {
        try({
          x <- as.numeric(unlist(r[,i+1]))
          fit <- dccfit(dccspecs, data = data.frame(x,btc))
              
          k <- nrow(fit@mfit$matcoef)
          ll <- fit@mfit$llh
              
          if(!is.null(k) ||  !is.null(ll)) {
            index <- ((p-1)*27) + ((q-1)*9) + ((d-1)*3) + m
            print(index)
                
            m.aic[index, i] <- (2*k - 2*ll) / (2*1827)
            m.bic[index, i] <- (2*log(k) - 2*ll) / (2*1827)
            m.LogLik[index, i] <- ll
            }
        })
        }
        }
      }
  
  models <- c("DCC", "ADCC", "FDCC")
  nms <- c(1:ncols)
  for (p in 1:p_max) {
    for (q in 1:q_max) {
      for(m in 1:m_max) {
        nm <- models[m] + "(" + as.character(p) + ","+ as.character(q) +")"
        nms[(p*(q_max+1)*m_max) + (q*m_max) + m] <-  nm
      }
    }
  }
  
  colnames(m.aic) <- nms
  colnames(m.bic) <- nms
  colnames(m.LogLik) <- nms
  
  t.m.aic <- t(m.aic)
  t.m.bic <- t(m.bic)
  t.m.LogLik <- t(m.LogLik)
  
  return(list(aic = t.m.aic, 
              bic = t.m.bic, 
              loglik = t.m.LogLik))
}

