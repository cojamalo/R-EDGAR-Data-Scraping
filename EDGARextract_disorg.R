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
ticker = "JNJ"
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
            new_row = data.frame(date=table$`Filing Date`[i], form=table$Filings[i], url_base=new_url, url_index=new_url_index)
            url_data = rbind(url_data, new_row)
        }
}
url_data = url_data %>% mutate_all(as.character)
rm(new_row, accno, action, base, CIK, CIK_code, count, directory, final, Find, i, new_url, owner, resp, start_date, ticker, type)


# Build reference word list
regex_cond1 = "^net income$"
regex_cond2 = "gross profit"
regex_cond3 = "total revenue"
regex_cond4 = "total cost of revenue"
regex_cond5 = "^diluted \\(per share\\)$"
for(word in earnings_list) { regex_cond1 = paste0(regex_cond1,"|",word) }
for(word in gross_list) { regex_cond2 = paste0(regex_cond2,"|",word) }
for(word in rev_list) { regex_cond3 = paste0(regex_cond3,"|",word) }
for(word in cost_list) { regex_cond4 = paste0(regex_cond4,"|",word) }
for(word in eps_list) { regex_cond5 = paste0(regex_cond5,"|",word) }

regex_units_mil = "\\$ in million|\\(usd \\$\\)in million"
regex_units_th = "\\$ in thousand|\\(usd \\$\\)in thousand"



raw_fin_hist = build_sales_hist(url_data)
rm(output)
head(raw_fin_hist)

#write.csv(raw_fin_hist,file='untidy_fin_hist.csv', sep = ",", row.names = FALSE)

library(data.table)
library(lubridate)
#raw_fin_hist = fread('untidy_fin_hist.csv')
tidy_fin_hist = raw_fin_hist %>% tbl_df %>% 
    gather(date, value, -record) %>% 
    select(date, record, value) %>% 
    #filter(!is.na(value)) %>% 
    spread(record, value) %>%
    select(date, form, revenue_mil_dol, net_earnings_mil_dol, diluted_eps_dol_per_share)
tidy_fin_hist$date = ymd(tidy_fin_hist$date)
tidy_fin_hist$form = factor(tidy_fin_hist$form)
head(tidy_fin_hist)
fin_hist = tidy_fin_hist %>% 
    mutate_if(is.character, as.numeric) %>%
    mutate_at(.vars = vars(revenue_mil_dol, net_earnings_mil_dol, diluted_eps_dol_per_share), .funs = funs(ifelse(form == "10-K", . - (lag(.,1) + lag(.,2) + lag(.,3)), .))) %>%
    arrange(desc(date))

head(fin_hist)
head(url_data,4)
system(paste0("open -a Safari ", url_data$url_base[1],"/R2.htm"))