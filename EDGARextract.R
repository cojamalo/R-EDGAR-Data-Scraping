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

## Input settings
ticker = "WYNN"
start_date = "2014-01-01" # full year date when xml and htm data started beign used

# Global variables
stopifnot(is.character(ticker))
directory = "http://www.sec.gov/cgi-bin/browse-edgar?"
download.file(url="https://www.sec.gov/include/InstanceReport.xslt", destfile = "temp/style.xslt", method="curl")

CIK = ticker
owner = "exclude"
action = "getcompany"
Find = "Search"
type = "10"
count = "40" # Options: 100, 80, 40, 20 - 100 filings is 23 years, 40 filings is 10 years

# Create inital EDGAR search url based on settings above
final = paste0(directory,"action=",action,"&CIK=",CIK, "&type=",10,"&owner=",owner, "&count=",count)

# extract the CIK code
resp = read_html(final)
CIK_code = resp %>%
    html_nodes("#documentsbutton") %>%
    .[[1]] %>% html_attr("href")
CIK_code = str_extract(CIK_code, '(?<=data\\/)[^\\/]+')

# Extract the search result table    
table = resp %>% 
    html_nodes(".tableFile2") %>%
    html_table()

# Extract the accensio nnumbers as new columns to table
table = table[[1]] %>% mutate(Acc_No = str_extract(Description, '(?<=Acc-no: )[^\\s]+'), Acc_No_code = gsub("-","",Acc_No))

# Create url dataframe with list of base urls and index urls for all forms on table
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

# # Extract the direct form link from the index url for each form in url_data
# form_urls = c()
# for (i in 1:nrow(url_data)) {
#     form_file = read_html(url_data$url_index[i]) %>%
#                     html_nodes(".tableFile") %>%
#                     .[[1]] %>%
#                     html_table %>%
#                     filter(Type == "10-K" | Type == "10-Q") %>%
#                     .$Document
#     form_url = paste0(url_data$url_base[i],"/",form_file) 
#     if (GET(form_url)$status_code != 200) {
#         print("error - bad link")
#         break
#         }
#     form_urls = c(form_urls, form_url)
# }
# url_data = cbind(url_data, form_urls)
# url_data$form_urls = as.character(url_data$form_urls)
# rm(form_urls)

# Extract table within form matching key words
earnings_list = c("net loss", "net earnings", "net income including noncontrolling interests", "consolidated net income","net income (loss)")
gross_list = c("gross margin", "income from operations", "operating income", "income before income taxes", "operating income", "loss from operations", "total revenues and other income", "income (loss) before income taxes")
rev_list = c("sales to customers", "sales and other operating revenue","net sales","total revenues","revenue", "total revenues and other income","total net sales and revenue","total operating revenues","net revenues","revenues", "total revenue, net of interest expense")
cost_list = c("cost of products sold", "cost of sales", "cost of revenues", "operating expenses", "costs and other deductions", "costs and expenses", "interest expense", "total operating expenses", "total noninterest expense")

regex_cond1 = "net income"
regex_cond2 = "gross profit"
regex_cond3 = "total revenue"
regex_cond4 = "cost of revenue"
for(word in earnings_list) { regex_cond1 = paste0(regex_cond1,"|",word) }
for(word in gross_list) { regex_cond2 = paste0(regex_cond2,"|",word) }
for(word in rev_list) { regex_cond3 = paste0(regex_cond3,"|",word) }
for(word in cost_list) { regex_cond4 = paste0(regex_cond4,"|",word) }

# table_list = list()
# for (i in 1:nrow(url_data)) {
#     #download.file(url_data$form_urls[i], destfile = "temp/temp_form.htm")
#     form_data = read_html(url_data$form_urls[i]) %>%
#         html_nodes("table") 
#     for (j in 1:length(form_data)) {
#         form_table = form_data[j] %>%
#             html_table(fill=TRUE) %>%
#             .[[1]] %>%
#             apply(2, function(x) {gsub("[\r\n]", "", x)}) %>% 
#             as.data.frame(stringsAsFactors=FALSE) %>%
#             mutate_if(is.character, tolower) %>%
#             apply(2, function(x) {gsub("\\s+", " ", str_trim(x))})  %>%
#             as.data.frame(stringsAsFactors=FALSE)
#         if ((any(grepl(regex_cond1, form_table, ignore.case = TRUE)) & any(grepl(regex_cond2, form_table, ignore.case = TRUE)) & any(grepl(regex_cond3, form_table, ignore.case = TRUE)))) {
#             print(url_data$form_urls[i])
#             table_list[[url_data$form_urls[i]]] = form_table
#             break
#         }
#     }
# }
# 
# print(paste("All revenue tables found for all urls?:",as.character(nrow(url_data) == length(table_list))))
# # Troubleshooting
# url_data$form_urls[!(url_data$form_urls %in% names(table_list))]
# 
# # troubleshoot specific form
# form_data = read_html("https://www.sec.gov/Archives/edgar/data/34088/000119312512078102/d257530d10k.htm") %>% html_nodes("table") 
# 
# for (j in 1:length(form_data)) {
#     form_table = form_data[j] %>%
#         html_table(fill=TRUE) %>%
#         .[[1]] %>%
#         apply(2, function(x) {gsub("[\r\n]", "", x)}) %>% 
#         as.data.frame(stringsAsFactors=FALSE) %>%
#         mutate_if(is.character, tolower) %>%
#         apply(2, function(x) {gsub("\\s+", " ", str_trim(x))})  %>%
#         as.data.frame(stringsAsFactors=FALSE)
#     if ((any(grepl(regex_cond1, form_table, ignore.case = TRUE)) &
#          any(grepl(regex_cond2, form_table, ignore.case = TRUE)) &
#          any(grepl(regex_cond3, form_table, ignore.case = TRUE)) &
#          any(grepl(regex_cond4, form_table, ignore.case = TRUE)))) {
#         # check if any match 1st list
#         print("Yes!")
#         print(j)
#          
#     } 
#         
# }


build_sales_hist = function(url_df) {
    first_col <<- c("revenue", "cost_of_revenue", "gross_profit", "net_earnings")
    for (i in 1:nrow(url_df)) {
        htm_check = paste0(url_df$url_base[i],"/R1.htm")
        xml_check = paste0(url_df$url_base[i],"/R1.xml")
        print(url_df$url_base[i])
        if (GET(htm_check)$status_code == 200) {
            print("Is a htm version.")
            data = find_table_R_htm(url_df[i,])
        }
        else if (GET(xml_check)$status_code == 200) {
            print("Is an xml version.")
            data = find_table_R_xml(url_df[i,])  
        }
        else {
            data == 0
        }
        
        if (data == 0) {}
        else {
            data[,2] = as.numeric(gsub("\\D", "", data[,2]))
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
    for (i in 1:200) {
        R_url = paste0(url_df_row$url_base,"/R",as.character(i),".htm")
        table = read_html(R_url) %>% 
            html_nodes(".report") %>%
            html_table(fill=TRUE) %>%
            .[[1]]
        if (names(table)[1] == names(table)[2]) {
            table[,2] = NULL        
        }
        for (j in 1:ncol(table)) {
            colnames(table)[j] = as.character(j)
        }    
        
        table = table %>%
            apply(2, function(x) {gsub("[\r\n]", "", x)}) %>% 
            as.data.frame(stringsAsFactors=FALSE) %>%
            mutate_if(is.character, tolower) %>%
            apply(2, function(x) {gsub("\\s+", " ", str_trim(x))})  %>%
            as.data.frame(stringsAsFactors=FALSE)
        names(table) = c("record", url_df_row$date)
        if ((any(grepl(regex_cond1, table, ignore.case = TRUE)) &
             any(grepl(regex_cond2, table, ignore.case = TRUE)) &
             any(grepl(regex_cond3, table, ignore.case = TRUE)) &
             any(grepl(regex_cond4, table, ignore.case = TRUE)))) {
            row1=which(apply(table, 2, function(x) {grepl(regex_cond1, x, ignore.case = TRUE)}))[1]
            row2=which(apply(table, 2, function(x) {grepl(regex_cond2, x, ignore.case = TRUE)}))[1]
            row3=which(apply(table, 2, function(x) {grepl(regex_cond3, x, ignore.case = TRUE)}))[1]
            row4=which(apply(table, 2, function(x) {grepl(regex_cond4, x, ignore.case = TRUE)}))[1]
            table = table[c(row3,row4,row2,row1),]
            table[,1] = first_col    
            names(table) = c("record", url_df_row$date)
            return(table[,1:2])
        }
    }
    return (0)
}

find_table_R_xml = function(url_df_row) {
    style <- read_xml("temp/style.xslt", package = "xslt")
    for (i in 1:200) {
        R_url = paste0(url_df_row$url,"/R",as.character(i),".xml")
        download.file(url=R_url, destfile = "temp/doc.xml", method="curl")
        doc <- read_xml("temp/doc.xml", package = "xslt")
        table <- xml_xslt(doc, style) %>% 
            as.character %>%
            read_html %>% 
            html_nodes(".report") %>%
            html_table(fill=TRUE) %>%
            .[[1]]
        if (names(table)[1] == names(table)[2]) {
            table[,2] = NULL        
        }
        for (j in 1:ncol(table)) {
                colnames(table)[j] = as.character(j)
        }    
        table = table %>%
            apply(2, function(x) {gsub("[\r\n]", "", x)}) %>% 
            as.data.frame(stringsAsFactors=FALSE) %>%
            mutate_if(is.character, tolower) %>%
            apply(2, function(x) {gsub("\\s+", " ", str_trim(x))})  %>%
            as.data.frame(stringsAsFactors=FALSE)
        names(table) = c("record", url_df_row$date)
        if ((any(grepl(regex_cond1, table, ignore.case = TRUE)) &
             any(grepl(regex_cond2, table, ignore.case = TRUE)) &
             any(grepl(regex_cond3, table, ignore.case = TRUE)) &
             any(grepl(regex_cond4, table, ignore.case = TRUE)))) {
            row1=which(apply(table, 2, function(x) {grepl(regex_cond1, x, ignore.case = TRUE)}))[1]
            row2=which(apply(table, 2, function(x) {grepl(regex_cond2, x, ignore.case = TRUE)}))[1]
            row3=which(apply(table, 2, function(x) {grepl(regex_cond3, x, ignore.case = TRUE)}))[1]
            row4=which(apply(table, 2, function(x) {grepl(regex_cond4, x, ignore.case = TRUE)}))[1]
            table = table[c(row3,row4,row2,row1),]
            table[,1] = first_col
            names(table) = c("record", url_df_row$date)
            return(table[,1:2])
        }
    }
    return (0)
}


untidy_fin_hist = build_sales_hist(url_data)
head(untidy_fin_hist)


# write.csv(untidy_fin_hist,file='untidy_fin_hist.csv', sep = ",", row.names = FALSE)
# 
# library(data.table)
# untidy_fin_hist = fread('untidy_fin_hist.csv')
# untidy_fin_hist = untidy_fin_hist %>% tbl_df %>% gather(date, value, -record) %>% select(date, record, value) %>% filter(!is.na(value)) %>% spread(record, value)
# unformated_fin_hist = untidy_fin_hist[,apply(untidy_fin_hist, 2, function(x) {mean(is.na(x))}) == 0]
# library(lubridate)
# unformated_fin_hist$date = gsub("\\.|[a-z]","", unformated_fin_hist$date) %>% ymd
# fin_hist = unformated_fin_hist %>% mutate_if(is.character, funs(gsub("\\D|^\\.", "", .))) %>% mutate_if(is.character, as.numeric)
# fin_hist_q = fin_hist %>% filter(month(date) != 10)
# fin_hist_k_to_q= fin_hist %>% mutate_if(is.numeric,funs(. - (lag(.,1) + lag(.,2) + lag(.,3))))  %>% filter(month(date) == 10)
# final = rbind(fin_hist_q, fin_hist_k_to_q) %>% arrange(desc(date))
# 
# 
# 
# table = read_html("https://www.sec.gov/Archives/edgar/data/200406/000020040617000006/R4.htm") %>% 
#     html_nodes(".report") %>%
#     html_table(fill=TRUE) %>%
#     .[[1]]
# if (names(table)[1] == names(table)[2]) {
#     table[,2] = NULL        
# }
# for (i in 1:ncol(table)) {
#     colnames(table)[i] = as.character(i)
# }    
# 
# table = table %>%
#     apply(2, function(x) {gsub("[\r\n]", "", x)}) %>% 
#     as.data.frame(stringsAsFactors=FALSE) %>%
#     mutate_if(is.character, tolower) %>%
#     apply(2, function(x) {gsub("\\s+", " ", str_trim(x))})  %>%
#     as.data.frame(stringsAsFactors=FALSE)
# names(table) = c("record", url_df_row$date)
# if ((any(grepl(regex_cond1, table, ignore.case = TRUE)) &
#      any(grepl(regex_cond2, table, ignore.case = TRUE)) &
#      any(grepl(regex_cond3, table, ignore.case = TRUE)) &
#      any(grepl(regex_cond4, table, ignore.case = TRUE)))) {
#     row1=which(apply(table, 2, function(x) {grepl(regex_cond1, x, ignore.case = TRUE)}))[1]
#     row2=which(apply(table, 2, function(x) {grepl(regex_cond2, x, ignore.case = TRUE)}))[1]
#     row3=which(apply(table, 2, function(x) {grepl(regex_cond3, x, ignore.case = TRUE)}))[1]
#     row4=which(apply(table, 2, function(x) {grepl(regex_cond4, x, ignore.case = TRUE)}))[1]
#     table = table[c(row3,row4,row2,row1),]
#     if (!exists("first_col")) {
#         first_col <<- table[,1]     
#     }
#     else {
#         table[,1] = first_col    
#     }
#     names(table) = c("record", url_data$date[i])
#     return(table[,1:2])
# }


