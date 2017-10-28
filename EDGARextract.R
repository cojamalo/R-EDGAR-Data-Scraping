#' @title Find 10-Q and 10-K Earnings Report Values - Revenue, Net Earnings, Diluted EPS
#'
#' @description
#' Scraps EDGAR website to find 10-Q and 10-K information for Revenue,
#' Net Earnings, and Dluted EPS on company earnings statements. Only designed to 
#' work for only a few tickers including:
#' JNJ, AAPL, TSLA, FB, COST, XOM, GM, BAC, AAL, WYNN, WMT, QCOM
#' @param ticker ticker symbol for company of interest
#' @param start_date earliest date of records to search
#' @importFrom tidyverse stringr RCurl httr rvest data.table lubridate
#' @return generates a tidy data frame of 10-Q and 10-K. For each observation,
#' returns: data, form type (10-Q, 10-K), revenue, netearnings, diluted EPS
#' @examples
#' find_earnings_reports(ticker="AAPL")
#' @name find_earnings_reports
NULL
#' @export

## Import Packages
library(tidyverse)
library(stringr)
library(RCurl)
library(httr)
library(rvest)
library(data.table)
library(lubridate)

## Set GLOBAL string references
earnings_list = c("^net loss$", "net earnings", 
                  "(net income attributable to)(?! noncontrolling interests)", 
                  "consolidated net income","^net income \\(loss\\)$",
                  "net income \\(loss\\) attributable to parent")
gross_list = c("gross margin", "income from operations", "operating income", 
               "income before income taxes", "operating income", 
               "net income \\(loss\\) before equity in net loss of unconsolidated entity", 
               "net income \\(loss\\) before equity in net loss of unconsolidated entity",
               "income \\(loss\\) before income taxes",
               "net loss before equity in net loss of unconsolidated entity", "net loss",
               "loss from operations", "income \\(loss\\) from operations")
rev_list = c("total other income, net","total other income", "sales to customers", 
             "sales and other operating revenue","total revenues",
             "total revenues and other income","total net sales and revenue",
             "total operating revenues","net revenues", "net sales",
             "total revenue, net of interest expense", 
             "^revenue$", "revenues, total", "^revenues$")
cost_list = c("cost of products sold", "cost of sales", "total cost of revenues", 
              "cost of revenue", "cost of revenues","operating expenses", 
              "costs and other deductions", "costs and expenses", "interest expense", 
              "total operating expenses", "total noninterest expense", "loss from operations",
              "merchandise costs")

eps_list = c("diluted \\(in dollars per share\\)", "^diluted$",
             "net loss per share of common stock attributable to common stockholders, basic and diluted",
             "net loss per share of common stock, basic and diluted",
             "net income \\(loss\\) per share of common stock, diluted",
             "diluted net earnings per share of common stock",
             "diluted net earnings \\(loss\\) per share of common stock",
             "earnings per common share - assuming dilution \\(dollars\\)",
             "diluted earnings per common share",
             "diluted earnings \\(in dollars per share\\)",
             "diluted \\(usd per share\\)", "diluted \\(earnings per share\\)",
             "earnings per share, diluted", "basic \\(in usd per share\\)",
             "diluted earnings per share attributable to",
             "net loss per share - basic and diluted", "net income \\(loss\\) per share - diluted",
             "assuming dilution \\(in dollars per share\\)",
             "earnings per share asumming dilution \\(in dollars per share\\)",
             "assuming dilution", "net earnings per share attributable to",
             "diluted net income per share", "diluted net income \\(loss\\) per share",
             "diluted net loss per share")

## Helper Functions

find_table_R_htm = function(url_df_row) {
    ## Function that builds candidate urls and downloads them from EDGAR,
    ## and then checks to see if table values match desired references to identify
    ## 10-Q and 10-K forms
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
        
        if (any(apply(table, 2, function(x) {str_detect(x, regex_cond1)}),na.rm=TRUE) &
            #any(apply(table, 2, function(x) {str_detect(x, regex_cond2)}),na.rm=TRUE) &
            any(apply(table, 2, function(x) {str_detect(x, regex_cond3)}),na.rm=TRUE) 
            #any(apply(table, 2, function(x) {str_detect(x, regex_cond4)}),na.rm=TRUE)
        ) {
            table[table==""]=NA
            table=table[,!apply(table, 2, function(x) {mean(is.na(x))}) > 0.60]
            if (any(apply(table, 2, function(x) {grepl(regex_units_mil, x, ignore.case = TRUE)}))) {
                units = "millions"    
            }
            else if (any(apply(table, 2, function(x) {grepl(regex_units_th, x, ignore.case = TRUE)}))) {
                units = "thousands"
            }
            else {
                units = "unknown"  
            }
            row1=which(apply(table, 2, function(x) {str_detect(x, regex_cond1)}), arr.ind=T)
            for (i in 1:nrow(row1)) {
                if (is.na(table[row1[i,1] ,(row1[i,2])+1])) {}
                else {
                    row1 = row1[i,1]
                    break
                }
            }
            if (class(row1) == "matrix") {row1 = row1[1,1]}
            #row2=which(apply(table, 2, function(x) {grepl(regex_cond2, x, ignore.case = TRUE)}), arr.ind=T)
            row3=which(apply(table, 2, function(x) {str_detect(x, regex_cond3)}), arr.ind=T)
            for (i in 1:nrow(row3)) {
                if (is.na(table[row3[i,1],row3[i,2]+1])) {}
                else {
                    row3 = row3[i,1]
                    break
                }
            }
            if (class(row3) == "matrix") {row3 = row3[1,1]}
            if(str_detect(table, "net sales") & str_detect(table, "total revenue")) {
                row3 = which(apply(table, 2, function(x) {str_detect(x, "total revenue")}), arr.ind=T)[1,1]
            }
            #row4=which(apply(table, 2, function(x) {grepl(regex_cond4, x, ignore.case = TRUE)}), arr.ind=T)
            row5=which(apply(table, 2, function(x) {str_detect(x, regex_cond5)}), arr.ind=T)
            if (!is.na(table[row5[1,1],row5[1,2]+1])) {
                row5 = row5[1,1]
            }
            else {
                for (i in 1:nrow(row5)) {
                    for (j in 1:3) {
                        if(!is.na(table[row5[i,1]+j,row5[i,2]+1])) {
                            row5 = row5[i,1]+j
                            found = TRUE
                            break
                        }   
                    }
                    if (found) {break}    
                }
            }
            if (class(row5) == "matrix") {row5 = row5[1,1]}
            table = table[c(row3,row1,row5),]
            table[,1] = first_col    
            table = cbind(table[,1:2], rep(units, 3))
            names(table) = c("record", url_df_row$date, "units")
            return(table)
        }
    }
    print("No table found")
    return (0)
}

build_sales_hist = function(url_df) {
    ## Function that tidies response from find_table_R_htm to prepare to construct
    ## tidy data frame from raw html table
    first_col <<- c("revenue_mil_dol", "net_earnings_mil_dol", "diluted_eps_dol_per_share")
    for (i in 1:nrow(url_df)) {
        htm_check = paste0(url_df$url_base[i],"/R1.htm")
        xml_check = paste0(url_df$url_base[i],"/R1.xml")
        print(url_df$url_base[i])
        if (GET(htm_check)$status_code == 200) {
            print("Is a htm version.")
            data = find_table_R_htm(url_df[i,])
        }
        else {
            data == 0
        }
        
        if (data == 0) { 
            print("Error! - No data returned - Missing or unknown revenue data")
            stop()
        }
        else {
            data_out = data
            data_out = data %>%
                mutate(neg = apply(data[,1:2], 2, function(x) {grepl("\\(.*\\)",x)})[,2],
                       no_sym = ifelse(is.na(str_extract(.[[2]],"([0-9]{1,3},)*[0-9]{1,3}\\.[0-9]*")),
                                       gsub("\\D","",.[[2]]),
                                       str_extract(.[[2]],"([0-9]{1,3},)*[0-9]{1,3}\\.[0-9]*")),
                       no_sym = gsub(",","",no_sym),
                       fixed = ifelse(neg, paste0("-", no_sym), no_sym)) %>%
                select(record, fixed)
            colnames(data_out)[2] = colnames(data)[2]
            data_out[,2] = as.numeric(data_out[,2])
            if (data$units[1] == "millions") {
                data_out[1:2,2] = data_out[1:2,2] * 10**0 
            }
            else if (data$units[1] == "thousands") {
                data_out[1:2,2] = data_out[1:2,2] * 10**-3 
            }
            else {
                data_out[1:2,2] = data_out[1:2,2] * 10**-6
                print("Warning! Units not explicately found!") 
                
            }
            data_out[,2] = as.character(data_out[,2])
            form_row = data.frame("form",url_df$form[i])
            names(form_row) = names(data_out)
            data_out = bind_rows(form_row,data_out)
            
            if (!exists("output")) {
                output = data_out
            }
            else {
                output = left_join(output, data_out, by="record")
            }
        }
    } 
    return (output)
}

## Main Function

find_earnings_reports = function(ticker = "JNJ", start_date = "2014-01-01") {
    # Make contact with EDGAR
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
    # head(raw_fin_hist)
    # write.csv(raw_fin_hist,file='untidy_fin_hist.csv', sep = ",", row.names = FALSE)
    #raw_fin_hist = fread('untidy_fin_hist.csv')
    tidy_fin_hist = raw_fin_hist %>% tbl_df %>% 
        gather(date, value, -record) %>% 
        select(date, record, value) %>% 
        #filter(!is.na(value)) %>% 
        spread(record, value) %>%
        select(date, form, revenue_mil_dol, net_earnings_mil_dol, diluted_eps_dol_per_share)
    tidy_fin_hist$date = ymd(tidy_fin_hist$date)
    tidy_fin_hist$form = factor(tidy_fin_hist$form)
    # head(tidy_fin_hist)
    fin_hist = tidy_fin_hist %>% 
        mutate_if(is.character, as.numeric) %>%
        mutate_at(.vars = vars(revenue_mil_dol, net_earnings_mil_dol, diluted_eps_dol_per_share), .funs = funs(ifelse(form == "10-K", . - (lag(.,1) + lag(.,2) + lag(.,3)), .))) %>%
        arrange(desc(date))
    #system(paste0("open -a Safari ", url_data$url_base[1],"/R2.htm"))
    return(fin_hist)
    #head(url_data,4)
}




