

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

my_garch_selector <- function(vars, p_max, q_max, models, submodels) {
  
  m_max <- length(models) # number of model types
  ncols <- (p_max+1)*(q_max+1)*length(models) # number of model combinations
  m <- matrix(nrow = ncol(vars), ncol = ncols)
  rownames(m) <- colnames(vars)
  
  m.aic    <- m
  m.bic    <- m
  m.LogLik <- m
  
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      for(m in 1:m_max) {
        ## GARCH model
        uspec <- ugarchspec(variance.model = list(model = models[m], garchOrder = c(p,q), submodel = submodels[m]), 
                            mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
                            distribution.model = "std")
        
        j <- (p*(q_max+1)*m_max) + (q*m_max) + m
        print(j + ' out of ' + ncols)
        
        for (i in 1:ncol(vars)) {
          x <- as.numeric(unlist(r[,i]))
          fit <- ugarchfit(uspec, x)
          
          k <- nrow(fit@fit$matcoef)
          ll <- fit@fit$LLH
          
          if(!is.null(k) ||  !is.null(ll)) {
            m.aic[i,j] <- 2*k - 2*ll
            m.bic[i,j] <- 2*log(k) - 2*ll
            m.LogLik[i,j] <- ll
          }
        }
      }
    }
  }
  
  nms <- c(1:ncols)
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      for(m in 1:m_max) {
        nm <- submodels[m] + "(" + as.character(p) + ","+ as.character(q) +")"
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

