# R-EDGAR-Data-Scraping
Scripts for extracting financial data from EDGAR filings for public companies by company ticker

This script is currently in development. It **does not** work for all tickers.

__Title:__ Find 10-Q and 10-K Earnings Report Values - Revenue, Net Earnings, Diluted EPS

__Description:__
Scraps EDGAR website to find 10-Q and 10-K information for Revenue,
Net Earnings, and Diluted EPS on company earnings statements. Currently only designed to 
work for a only a select few tickers including:
JNJ, AAPL, TSLA, FB, COST, XOM, GM, BAC, AAL, WYNN, WMT, QCOM

__Returns:__ a tidy data frame of 10-Q and 10-K. For each observation,
#' returns: data, form type (10-Q, 10-K), revenue, netearnings, diluted EPS

__Dependencies:__ tidyverse stringr RCurl httr rvest data.table lubridate

__Main Function Call__: find_earnings_reports(ticker, start_date)

__Example__: find_earnings_reports(ticker="AAPL")
