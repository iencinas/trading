#construir url
library(RCurl)
library(XML)
library(tidyverse)
library(stringr)
library(qdapRegex)


library(rlist)
library(lubridate)
library(scales)

rm(symbols)
symbols <- readRDS(file = 'Nasdaq/data/symbols.RDS')
head(symbols)
symbols <- symbols[,c('Symbol','Name','Exchange')]


symbol.list <- symbols[,1]


startTime <- Sys.time()

temp.df <- data.frame()
for(i in 1:length(symbol.list)){

url <- paste0("https://finance.yahoo.com/quote/",symbol.list[i],"/profile?p=",symbol.list[i])
theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )
doc <- htmlParse(theurl, asText=TRUE)
list <- sapply(doc["//p"], xmlValue)
list <- list[grepl("Sector",list)]


a1 <- as.character(qdapRegex::ex_between(list, "Sector(s):", "Industry")[1])
a2 <- as.character(qdapRegex::ex_between(list, "Industry:", "Full")[1])
a3 <- str_extract(list, '\\b\\w+$')

temp.df <- rbind(temp.df,data.frame(Symbol=symbol.list[i]),
                 Sector=a1,
                 Industry=a2,
                 Employees=a3)
cat('.')
if(i%%100==0)print(paste0(i,'  ',Sys.time()-startTime))
}


symbols_extra <- symbols%>%left_join(temp.df)
saveRDS(object = symbols_extra,file = 'Nasdaq/data/symbols_extra.RDS')


sys.time()
Sys.time()-startTime

url <- "https://es.finance.yahoo.com/quote/AAPL/key-statistics?p=AAPL"

theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )

aux <- readHTMLTable(theurl)
length(aux)
res <- do.call(rbind,aux[-c(1,length(aux))])
head(res)
colnames(res) <- c('name','value')



a1 <- as.character(qdapRegex::ex_between(list, "Sector(s):", "Industry")[1])
a2 <- as.character(qdapRegex::ex_between(list, "Industry:", "Full")[1])

strsplit(a1,split = "Sector:")[1]
strsplit(a1,split = "Sector(es):")[1]

url <- "https://es.finance.yahoo.com/quote/AAPL/analysis?p=AAPL"
theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )

aux <- readHTMLTable(theurl)

https://es.finance.yahoo.com/quote/AAPL/holders?p=AAPL


<div class="rating-text Arrow South Fw(b) Bgc($buy) Bdtc($buy)" tabindex="0" aria-label="2.2 en una escala de 1 a 5, donde 1 es Compra agresiva y 5 es Vender" style="width: 30px; left: calc(30% - 15px);">2.2</div>
  doc <- htmlParse(theurl, asText=TRUE)
list <- sapply(doc["//div class"], xmlValue)