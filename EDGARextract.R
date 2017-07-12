library(rvest)
library(tidyverse)
library(stringr)
library(xlsx)

ticker = "AAPL"
  stopifnot(is.character(ticker))
  directory = "http://www.sec.gov/cgi-bin/browse-edgar?"

 CIK = ticker
 owner = "exclude"
 action = "getcompany"
 Find = "Search"
 type = "10"
 count = "40" # Options: 100, 80, 40, 20 - 100 filings is 23 years, 40 filings is 10 years
final = paste0(directory,"action=",action,"&CIK=",CIK, "&type=",10,"&owner=",owner, "&count=",count)

resp = read_html(final)

CIK_code = resp %>%
    html_nodes("#documentsbutton") %>%
    .[[1]] %>% html_attr("href")

CIK_code = str_extract(CIK_code, '(?<=data\\/)[^\\/]+')
    
table = resp %>% 
    html_nodes(".tableFile2") %>%
    html_table()

table = table[[1]] %>% mutate(Acc_No = str_extract(Description, '(?<=Acc-no: )[^\\s]+'), Acc_No = gsub("-","",Acc_No))

base = "https://www.sec.gov/Archives/edgar/data/"
Q_urls = K_urls = c()
for (i in 1:nrow(table)) {
        accno = table$Acc_No[i]
        new_url = paste0(base,CIK_code,"/",accno,"/Financial_Report.xlsx")    
        
        if (grepl("10-Q", table$Filings[i])) {
            Q_urls = c(Q_urls, new_url)
        }
        else if (grepl("10-K", table$Filings[i])) {
            K_urls = c(K_urls, new_url)
        }
}



K_sheetName = "CONSOLIDATED STATEMENT OF EARNI"
Q_sheetName = "CONDENSED CONSOLIDATED STATEMEN"

get_xlsx_tables = function(url_list, sheetName) {
    for (url in url_list) {
        download.file(url=url, destfile = "/temp/finrepo.xlsx")
        table = read.xlsx("/temp/finrepo.xlsx", sheetName = sheetName)
        if (is.null(output)) {
        output = table    
        }
        else {
        output = cbind(output, table[,-1])    
        }
    }
    return (output)
}






