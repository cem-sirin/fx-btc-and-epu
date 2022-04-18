library(rmgarch)
library(zoo)
library(xts)

## The following code terates over all the dcc models under inspection
source('dcc_selection.R')

## AIC values for DCC models per currency
res$aic

## BIC values for DCC models per currency
res$bic

rm(res)
library(tsDyn)

install.packages('tsDyn')
citation(package = 'tsDyn')
options(citation.bibtex.max=999)
