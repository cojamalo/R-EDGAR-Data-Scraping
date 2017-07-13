library(xslt)
library(htmltab)
library(tidyverse)
library(stringr)
library(RCurl)
library(httr)
library(rvest)


setwd("/Users/cojamalo/Documents/GitHub/R-EDGAR-Data-Scraping")

ticker = "AAPL"
start_date = "2009-06-01" # date when xml and htm data started beign used
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

download.file(url="https://www.sec.gov/include/InstanceReport.xslt", destfile = "temp/style.xslt", method="curl")

base = "https://www.sec.gov/Archives/edgar/data/"

url_data = data.frame()
for (i in 1:nrow(table)) {
        accno = table$Acc_No[i]
        new_url = paste0(base,CIK_code,"/",accno)    
        
        if (table$`Filing Date`[i] >= start_date) {
            new_row = data.frame(date=table$`Filing Date`[i], url=new_url)
            url_data = rbind(url_data, new_row)
        }
}
url_data = url_data %>% mutate_all(as.character)

rm(new_row, accno, action, base, CIK, CIK_code, count, directory, final, Find, i, new_url, owner, resp, start_date, ticker, type)

build_sales_hist = function(url_df) {
    for (i in 1:nrow(url_df)) {
        url_check = paste0(url_df$url[i],"/R1.htm")
        print(url_df$url[i])
        if (GET(url_check)$status_code == 200) {
            print("Is a htm version.")
            data = find_table_R_htm(url_df[i,])
        }
        else {
            print("Is an xml version.")
            data = find_table_R_xml(url_df[i,])  
        }
        if (data == 0) {}
        else {
           if (!exists("output")) {
               output = data
           }
           else {
               output = left_join(output, data[,1:2], by="record")
           }
        }
    } 
    return (output)
}

find_table_R_htm = function(url_df_row) {
    for (i in 1:10) {
        R_url = paste0(url_df_row$url,"/R",as.character(i),".htm")
        resp_R = read_html(R_url)
        table = resp_R %>% 
            html_nodes(".report") %>%
            as.character %>%
            htmltab(1)
        names(table) = c("record", url_df_row$date)
        if (any(grepl("Net sales", table[1])) & any(grepl("Cost of sales", table[1])) & any(grepl("Gross margin", table[1]))) {
            return(table[,1:2])   
        }
    }
    return (0)
}

find_table_R_xml = function(url_df_row) {
    for (i in 1:10) {
        R_url = paste0(url_df_row$url,"/R",as.character(i),".xml")
        download.file(url=R_url, destfile = "temp/doc.xml", method="curl")
        doc <- read_xml("temp/doc.xml", package = "xslt")
        style <- read_xml("temp/style.xslt", package = "xslt")
        table <- xml_xslt(doc, style) %>% as.character() %>% read_html() %>% html_nodes(".report") %>% as.character %>% htmltab(1)
        names(table) = c("record", url_df_row$date)
        if (any(grepl("Net sales", table[1])) & any(grepl("Cost of sales", table[1])) & any(grepl("Gross margin", table[1]))) {
            return(table[,1:2])   
        }
    }
    return (0)
}


untidy_fin_hist = build_sales_hist(url_data)
write.csv(untidy_fin_hist,file='untidy_fin_hist.csv', sep = ",", row.names = FALSE)

library(data.table)
untidy_fin_hist = fread('untidy_fin_hist.csv')
untidy_fin_hist = untidy_fin_hist %>% tbl_df %>% gather(date, value, -record, -V1) %>% select(date, record, value) %>% filter(!is.na(value)) %>% spread(record, value)
unformated_fin_hist = untidy_fin_hist[,apply(untidy_fin_hist, 2, function(x) {mean(is.na(x))}) == 0]
library(lubridate)
unformated_fin_hist$date = gsub("\\.|[a-z]","", unformated_fin_hist$date) %>% ymd
fin_hist = unformated_fin_hist %>% mutate_if(is.character, funs(gsub("\\D|^\\.", "", .))) %>% mutate_if(is.character, as.numeric)




