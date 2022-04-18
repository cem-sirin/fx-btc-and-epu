## Selecting GARCH models
library(rugarch) # package that models GARCHs
source('my_garch_selector.R') # local function for iterating GARCH models

p_max <- 3 # max number of p 
q_max <- 3 # max number of p 

# we are implementing GARCH and GJR GARCH
models <- c("sGARCH", "fGARCH")
submodels <- c("GARCH", "GJRGARCH")

res <- my_garch_selector(r[,-1], p_max, q_max,models, submodels)
res$aic # Akaike Information Criterion for the respective GARCH models
res$bic # Bayesian Information Criterion for the respective GARCH models

res <- my_garch_selector(r[,9], 9, 0, models[1], submodels[1])
res$aic # Akaike Information Criterion for further Bitcoin ARCH models
res$bic # Bayesian Information Criterion for further Bitcoin ARCH models

rm(res, p_max, q_max, models, submodels, `+`, my_garch_selector)

citation(package = "rugarch")
citation(package = "rmgarch")
citation(package = "dccmidas")
citation(package = "ConnectednessApproach")
