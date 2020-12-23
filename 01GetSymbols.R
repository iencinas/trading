library("quantmod")
library(tidyverse)
library(RCurl)
httr::config(connecttimeout = 6)
symbols <- stockSymbols()
symbols <- symbols%>%filter(Exchange %in% c('NASDAQ','NYSE'),ETF=='FALSE')
saveRDS(object = symbols,file = 'Nasdaq/data/symbols.RDS')




