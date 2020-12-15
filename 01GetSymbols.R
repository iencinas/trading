library("quantmod")
library(tidyverse)

symbols <- stockSymbols()
symbols <- symbols%>%filter(Exchange %in% c('NASDAQ','NYSE'),ETF=='FALSE')
saveRDS(object = symbols,file = 'Nasdaq/data/symbols.RDS')
help("stockSymbols")
