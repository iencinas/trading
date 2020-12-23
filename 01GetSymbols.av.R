library(tidyverse)
library(jsonlite)


url <- "https://www.alphavantage.co/query?function=LISTING_STATUS&apikey=A7Q9IOL5MGIF3GQ8"
symbols.av <- read.csv(url)
head(symbols.av)
symbols.av <- symbols.av%>%dplyr::filter(assetType=='Stock',exchange %in% c('NASDAQ','NYSE'))
saveRDS(object = symbols.av,file = 'Nasdaq/data/symbols.av.RDS')




symbol.list <- symbols.av[,1]

temp.df <- data.frame()
i

symbol.list[i]
for(i in 1:length(symbol.list)){
  url <- paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=",symbol.list[i],"&apikey=A7Q9IOL5MGIF3GQ8")
  json.data <- fromJSON(url)
  aux <- data.frame(json.data)%>%select(-'Description')
  
  temp.df <- rbind(temp.df,aux)
  cat('.')
  if(i%%100==0) {
    print(paste0(i,'  ',Sys.time()-startTime))
    startTime <- Sys.time()
  }
  
}


symbols.av_extra <- symbols.av%>%left_join(temp.df,by=c("symbol"="Symbol"))
saveRDS(object = symbols.av_extra,file = 'data/symbols.av_extra.RDS')

