setwd('Desktop/code')

### Loading Data and Preliminary Data Analysis

library(readxl) ## loading data from excel
library(moments) ## to get summary statistics
library(tseries) ## for jarque-bera and adf test
library(dplyr) ## general purpose data manipultion

p <- read_excel('data/prices.xlsx') # load prices
r <- read_excel('data/log_returns.xlsx') # load returns

source('mysummarytable.R') # imports local function for summary table

mysummarytable(p[,-1]) # summary table for prices
mysummarytable(r[,-1]) # summary table for log returns

rm(mysummarytable)

### Correlation for log returns
r %>% select(-DATE) %>% as.matrix() %>% cor() # entire dataset
r %>% filter(DATE < "2020-03-11") %>% select(-DATE) %>% as.matrix() %>% cor() # pre-covid
r %>% filter(DATE >= "2020-03-11") %>% select(-DATE) %>% as.matrix() %>% cor() # post-covid

epu <- read_excel('epu.xlsx') ## load epu
epu %>% select(-Date) %>% as.matrix() %>% cor() # entire dataset

r %>% select(-BTCUSD)
