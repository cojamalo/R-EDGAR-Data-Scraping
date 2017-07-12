library(xslt)
library(rvest)
library(tidyverse)
library(stringr)
library(xlsx)
library(RCurl)

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
        new_url = paste0(base,CIK_code,"/",accno)    
        
        if (grepl("10-Q", table$Filings[i])) {
            Q_urls = c(Q_urls, new_url)
        }
        else if (grepl("10-K", table$Filings[i])) {
            K_urls = c(K_urls, new_url)
        }
}

build_sales_hist = function(url_list) {
    download.file(url="https://www.sec.gov/include/InstanceReport.xslt", destfile = "temp/style.xslt", method="curl")
    for (url in url_list) {
        url = paste0(url,"/R1.htm")
        print(url)
        if (url.exists(url)) {
            print("Is a htm version.")
            data = find_table_R_htm(url)
        }
        else {
            print("Is an xml version.")
            data = find_table_R_xml(url)  
        }
        if (data == 0) {}
        else {
           if (!exists("output")) {
               output = data
           }
           else {
               output = left_join(output, data[,1:2])
           }
        }
    } 
    return (output)
}

find_table_R_htm = function(url) {
    for (i in 1:10) {
        R_url = paste0(url,"/R",as.character(i),".htm")
        resp_R = read_html(R_url)
        table = resp_R %>% 
            html_nodes(".report") %>%
            html_table(fill=TRUE) %>%
            .[[1]]
        names(table) = c("one","two","three","four")
        if (any(grepl("Net sales", table[1])) & any(grepl("Cost of sales", table[1])) & any(grepl("Gross margin", table[1]))) {
            return(table)   
        }
    }
    return (0)
}

find_table_R_xml = function(url) {
    for (i in 1:10) {
        R_url = paste0(url,"/R",as.character(i),".xml")
        download.file(url=R_url, destfile = "temp/doc.xml", method="curl")
        doc <- read_xml("temp/doc.xml", package = "xslt")
        style <- read_xml("temp/style.xslt", package = "xslt")
        table <- xml_xslt(doc, style) %>% as.character() %>% read_html() %>% html_nodes(".report") %>% html_table(fill=TRUE)
        names(table) = c("one","two","three","four")
        if (any(grepl("Net sales", table[1])) & any(grepl("Cost of sales", table[1])) & any(grepl("Gross margin", table[1]))) {
            return(table)   
        }
    }
    return (0)
}




K_sheetNames = c("CONSOLIDATED STATEMENT OF EARNI", "CONSOLIDATED STATEMENTS OF OPER", "CONSOLIDATED_STATEMENTS_OF_OPE","Consolidated_Statements_of_Ope") 
Q_sheetName = "CONDENSED CONSOLIDATED STATEMEN"

get_xlsx_tables = function(url_list, sheetNames) {
    i = 0
    for (url in url_list) {
        i = i + 1
        download.file(url=url, destfile = paste0("temp/",as.character(i),".xlsx"), method="curl")
        
        wb = loadWorkbook(paste0("temp/",as.character(i),".xlsx"))
        sheetName = K_sheetNames[which(K_sheetNames %in% names(getSheets(wb)))]
        table = read.xlsx(paste0("temp/",as.character(i),".xlsx"), sheetName = sheetName)
        if (!exists("output")) {
        output = table    
        }
        else {
        output = left_join(output, table)    
        }
    }
    return (output)
}
get_xlsx_tables(K_urls, K_sheetNames)







