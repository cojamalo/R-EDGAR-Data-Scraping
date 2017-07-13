library(xslt)
library(htmltab)
library(tidyverse)
library(stringr)
library(RCurl)
library(httr)
library(rvest)

### Need to account for different revenue vs sales tables vs statement of income Alt
# 10-Q
# JNJ - CONSOLIDATED STATEMENTS OF EARNINGS - Sales to customers, Gross profit, NET EARNINGS - https://www.sec.gov/Archives/edgar/data/200406/000020040617000024/0000200406-17-000024-index.htm
# APPL - CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS - Net sales, Gross margin, Net income - https://www.sec.gov/Archives/edgar/data/320193/000162828017000717/0001628280-17-000717-index.htm
# TSLA - Consolidated Statements of Operations - Total revenues, Gross profit, Net loss -
# FB - CONSOLIDATED STATEMENTS OF INCOME - Revenue, Income from operations,Net income
# AOS - CONSOLIDATED STATEMENTS OF EARNINGS - Net sales, Gross profit, Net Earnings
# COST - CONSOLIDATED STATEMENTS OF INCOME - Total revenue, Operating Income, Net income including noncontrolling interests
# XOM - CONSOLIDATED STATEMENT OF INCOME - Total revenues and other income, Income before income taxes, Net income including noncontrolling interests
# GM - CONSOLIDATED INCOME STATEMENTS - Total net sales and revenue, Operating income, Net income
# BAC - Consolidated Statement of Income - Total revenue, net of interest expense, Income before income taxes, Net income
# AAL - CONSOLIDATED STATEMENTS OF OPERATIONS - Total operating revenues, Operating income, Net income
# WYNN - CONSOLIDATED STATEMENTS OF INCOME - Net revenues, Operating income, Net income
# WMT - Consolidated Statements of Income - Total revenues, Operating income, Consolidated net income
# QCOM - CONSOLIDATED STATEMENTS OF OPERATIONS - Total revenues, Operating income, Net income
# ALDR - Consolidated Statements of Operations - Revenues, Loss from operations, Net loss
# AAON - Consolidated Statements of Income - Net sales, Gross profit, Net income

setwd("/Users/cojamalo/Documents/GitHub/R-EDGAR-Data-Scraping")

ticker = "TSLA"
start_date = "2012-01-01" # full year date when xml and htm data started beign used
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

table = table[[1]] %>% mutate(Acc_No = str_extract(Description, '(?<=Acc-no: )[^\\s]+'), Acc_No_code = gsub("-","",Acc_No))

download.file(url="https://www.sec.gov/include/InstanceReport.xslt", destfile = "temp/style.xslt", method="curl")

base = "https://www.sec.gov/Archives/edgar/data/"

url_data = data.frame()
for (i in 1:nrow(table)) {
        accno = table$Acc_No[i]
        accno_code = table$Acc_No_code[i]
        new_url = paste0(base,CIK_code,"/",accno_code)
        new_url_index = paste0(base,CIK_code,"/",accno,"/",accno_code,"-index.htm")    
        
        if (table$`Filing Date`[i] >= start_date & (table$Filings[i] == "10-Q" | table$Filings[i] == "10-K") ) {
            new_row = data.frame(date=table$`Filing Date`[i], url_base=new_url, url_index=new_url_index)
            url_data = rbind(url_data, new_row)
        }
}
url_data = url_data %>% mutate_all(as.character)

rm(new_row, accno, action, base, CIK, CIK_code, count, directory, final, Find, i, new_url, owner, resp, start_date, ticker, type)

new_col = c()
for (i in 1:nrow(url_data)) {
    form_file = read_html(url_data$url_index[i]) %>%
                    html_nodes(".tableFile") %>%
                    .[[1]] %>%
                    html_table %>%
                    filter(Type == "10-K" | Type == "10-Q") %>%
                    .$Document
    form_url = paste0(url_data$url_base[i],"/",form_file) 
    if (GET(form_url)$status_code != 200) {
        print("error - bad link")
        break
        }
    new_col = c(new_col, form_url)
}
url_data = cbind(url_data, as.character(new_col))
rm(new_col)

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
    style <- read_xml("temp/style.xslt", package = "xslt")
    for (i in 1:10) {
        R_url = paste0(url_df_row$url,"/R",as.character(i),".xml")
        download.file(url=R_url, destfile = "temp/doc.xml", method="curl")
        doc <- read_xml("temp/doc.xml", package = "xslt")
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
untidy_fin_hist = untidy_fin_hist %>% tbl_df %>% gather(date, value, -record) %>% select(date, record, value) %>% filter(!is.na(value)) %>% spread(record, value)
unformated_fin_hist = untidy_fin_hist[,apply(untidy_fin_hist, 2, function(x) {mean(is.na(x))}) == 0]
library(lubridate)
unformated_fin_hist$date = gsub("\\.|[a-z]","", unformated_fin_hist$date) %>% ymd
fin_hist = unformated_fin_hist %>% mutate_if(is.character, funs(gsub("\\D|^\\.", "", .))) %>% mutate_if(is.character, as.numeric)
fin_hist_q = fin_hist %>% filter(month(date) != 10)
fin_hist_k_to_q= fin_hist %>% mutate_if(is.numeric,funs(. - (lag(.,1) + lag(.,2) + lag(.,3))))  %>% filter(month(date) == 10)
final = rbind(fin_hist_q, fin_hist_k_to_q) %>% arrange(desc(date))


CIK_code = resp %>%
    html_nodes("#documentsbutton") %>%
    .[[1]] %>% html_attr("href")

CIK_code = str_extract(CIK_code, '(?<=data\\/)[^\\/]+')

table = resp %>% 
    html_nodes(".tableFile2") %>%
    html_table()


