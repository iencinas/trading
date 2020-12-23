library(rvest)

url <- "https://finviz.com/quote.ashx?t=AAPL"
webpage <- read_html(url)
webpage %>% 
  html_nodes("table" )%>%.[[8]]%>%html_table(fill=TRUE)

n <- webpage %>% 
  html_nodes("table" )%>%.[10]%>%html_table(fill=TRUE)

n <- n[complete.cases(n),]
nrow(data.frame(n))

ll <- webpage %>% 
  html_nodes("table" )%>%.[11:30]%>%html_table(fill=TRUE)
do.call(rbind.data.frame, ll)

do.call(rbind.data.frame, n[complete.cases(n),])

webpage %>% 
  html_nodes(xpath= "/html/body/div[4]/div/table[3]/tbody/tr[4]/td/table" )%>%html_table(fill=TRUE)


symbols <- readRDS(file = 'Nasdaq/data/symbols_extra.RDS')
head(symbols)


symbol.list <- symbols[,1]


startTime <- Sys.time()
i=1
temp.df <- data.frame()
for(i in 1:length(symbol.list)){
  
  url <- paste0("https://finance.yahoo.com/quote/",symbol.list[i],"/key-statistics?p=",symbol.list[i])
  webpage <- read_html(url)
  table <- webpage %>%html_nodes("table" )%>%.[[1]]%>%html_table(fill=TRUE)
  
  aux <- data.frame(table)
  library(reshape2)
  library(lubridate)
  a <- melt(aux,id.vars = 'Var.1')
  b <- dcast(a,variable~Var.1)
  colnames(b) <- c('dt','Value','Value_EBITDA','Value_Revenue','F_P_E','Market_Cap','PEG','Price_book','Price_sales','P_E')
  b$dt <- substring(b$dt, 2)
  b$dt <- gsub("\\.","-",b$dt) 
  b$dt <- parse_date_time(b$dt, orders="mdy")
  
  b$Symbol <- 
    
    url <- "https://finance.yahoo.com/quote/AAPL?p=AAPL"
  webpage1 <- read_html(url)
  webpage1 %>% 
    html_nodes("table" )%>%.[[2]]%>%html_table(fill=TRUE)
  
  
  
  
  tbls <- html_nodes(webpage, "table[2]")
  
  
  /html/body/div[4]/div/table[2]
  
  webpage %>% 
    html_nodes(xpath= "/html/body/div[4]/div/table[2]" )%>%html_table(fill=TRUE)
  
  webpage %>% 
    html_nodes(xpath= '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table' )%>%html_table(fill=TRUE)
  i=3
  i=i+1
  webpage %>% 
    html_nodes("table" )%>%.[[i]]%>%html_table(fill=TRUE)
  i
  
  //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table
  content <- read_html(url)
  tables <- content %>% html_table(fill = TRUE)
  first_table <- tables[[1]]
  
  url <- paste0("https://finance.yahoo.com/quote/",symbol.list[i],"/profile?p=",symbol.list[i])
  url <- "https://finviz.com/quote.ashx?t=AAPL"
  ult <- "https://finance.yahoo.com/quote/AAPL/key-statistics?p=AAPL"
  theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )
  
  doc <- htmlParse(theurl, asText=TRUE)
  list <- sapply(doc["//Index"], xmlValue)
  list <- list[grepl("Sector",list)]
  
  
  
  a1 <- as.character(qdapRegex::ex_between(list, "Sector(s):", "Industry")[1])
  a2 <- as.character(qdapRegex::ex_between(list, "Industry:", "Full")[1])
  a3 <- as.character(str_extract(list, '\\b\\w+$'))
  
  
  /html/body/div[4]/div/table[2]
  
  webpage %>% 
    html_nodes(xpath= "/html/body/div[4]/div/table[2]" )%>%html_table(fill=TRUE)
  
  webpage %>% 
    html_nodes(xpath= '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table' )%>%html_table(fill=TRUE)
  i=3
  i=i+1
  webpage %>% 
    html_nodes("table" )%>%.[[i]]%>%html_table(fill=TRUE)
  i
  
  //*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[3]/div[1]/div[2]/div/div[1]/div[1]/table
  content <- read_html(url)
  tables <- content %>% html_table(fill = TRUE)
  first_table <- tables[[1]]
  
  url <- paste0("https://finance.yahoo.com/quote/",symbol.list[i],"/profile?p=",symbol.list[i])
  url <- "https://finviz.com/quote.ashx?t=AAPL"
  ult <- "https://finance.yahoo.com/quote/AAPL/key-statistics?p=AAPL"
  theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )
  
  doc <- htmlParse(theurl, asText=TRUE)
  list <- sapply(doc["//Index"], xmlValue)
  list <- list[grepl("Sector",list)]
  
  
  
  a1 <- as.character(qdapRegex::ex_between(list, "Sector(s):", "Industry")[1])
  a2 <- as.character(qdapRegex::ex_between(list, "Industry:", "Full")[1])
  a3 <- as.character(str_extract(list, '\\b\\w+$'))
  