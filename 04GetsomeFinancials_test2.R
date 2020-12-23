library(rvest)
library(curl)
library(reshape2)
library(lubridate)

symbols <- readRDS(file = 'Nasdaq/data/symbols_extra_clean.RDS')
head(symbols)


symbol.list <- symbols[,1]

# read_html(curl('http://google.com', handle = curl::new_handle("useragent" = "Mozilla/5.0")))

# uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
# session <- html_session("https://finance.yahoo.com/quote/", user_agent(uastring))

i=20
temp.df <- data.frame()
temp.df.finc <- data.frame()
startTime <- Sys.time()
for(i in 1:length(symbol.list)){
  cat('.')
  symbol.text <- symbol.list[i]
  robust1 <- 'a'
  skip=0
  while(robust1!='Var.1' & skip<10){
    url <- paste0("https://finance.yahoo.com/quote/",symbol.text,"/key-statistics?p=",symbol.text)
    webpage <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    table <- webpage %>%html_nodes("table" )%>%.[[1]]%>%html_table(fill=TRUE)
    webpage %>%html_nodes("table" )%>%.[[1]]%>%html_table(fill=TRUE)
    aux <- data.frame(table)  
    robust1 <- colnames(aux)[1]
    skip=skip+1
    if(robust1!='Var.1') {
      Sys.sleep(sample(40, 1) * 0.1)
      cat('-')
    }
  }
  
  if(skip==10) {
    cat('@')
    Sys.sleep(5)
  } else {
    
    a <- melt(aux,id.vars = 'Var.1')
    b <- dcast(a,variable~Var.1)
    colnames(b) <- c('Finc_dt','Value','Value_EBITDA','Value_Revenue','F_P_E','Market_Cap','PEG','Price_book','Price_sales','P_E')
    b$Finc_dt <- substring(b$Finc_dt, 2)
    b$Finc_dt <- gsub("\\.","-",b$Finc_dt) 
    b$Finc_dt <- parse_date_time(b$Finc_dt, orders="mdy")
    b$Symbol <- symbol.text
    
    temp.df.finc <- rbind(temp.df.finc,data.frame(b))
  }
  Sys.sleep(sample(5, 1) * 0.1)
  
  if(i%%10==0) {
    Sys.sleep(sample(10, 1) * 0.1)
    print(paste0(i,'  ',Sys.time()-startTime))
    startTime <- Sys.time()
  }
}



