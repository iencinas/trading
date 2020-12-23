library(tidyverse)
library(jsonlite)


url <- "https://www.alphavantage.co/query?function=LISTING_STATUS&apikey=A7Q9IOL5MGIF3GQ8"
symbols.av <- read.csv(url)

symbols.av <- symbols.av%>%dplyr::filter(assetType=='Stock',exchange %in% c('NASDAQ','NYSE'))
saveRDS(object = symbols.av,file = 'Nasdaq/data/symbols.av.RDS')

head(symbols.av)
symbols_e <- readRDS(file = 'Nasdaq/data/symbols_extra.RDS')
head(symbols_e)
symbols_e <- symbols_e%>%left_join(symbols.av[,c('symbol','status','ipoDate')],by=c('Symbol'='symbol'))
symbols_e <- symbols_e%>%dplyr::filter(Industry!='NULL')

saveRDS(symbols_e,'Nasdaq/data/symbols_extra_clean.RDS')


#exlore
symbols_e%>%dplyr::filter(grepl(symbol.text,x = Symbol))
symbols_e%>%dplyr::filter(grepl('AAC',x = Symbol))
symbols_e%>%group_by(Industry=='NULL',status)%>%summarise(n())

head(symbols_e%>%dplyr::filter(Industry=='NULL',is.na(status)),n=100)
head(symbols_e%>%dplyr::filter(is.na(status)),n=10)
tail(symbols_e%>%dplyr::filter(!is.na(status)))

symbols_e%>%group_by(Industry=='NULL',status)%>%summarise(n())