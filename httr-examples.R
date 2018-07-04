## -*- coding: utf-8 -*-
## httr-examples.R
## package for http(s) communication
## Author: René Locher
## Version: 2018-07-04

require(httr)
require(IDPmisc)
require(debug)
## mtrace(content)
## mtrace(content, FALSE)

pathIn <- "dat/"
url <- "https://api.prognolite.com/1.0/en-gb/timeseries/get?time_series=m-hist_meteoblue_fluntern_temp_f,m-hist_meteoblue_fluntern_windspeed_f&format=csv&from=1483228800&to=1488412800&count_per_page=480&page=6&resolution=1800"

options(width = 72)       ## width of default emacs
options(max.print = 1000) ## stops printing after 1000 values

token <- readRDS("E:/ZHAW/Private/Progno/code/private/PrognoliteToken.rds")
dataPage <-
    GET(url,
        query = list(access_token = token$credentials$access_token), verbose())

dat <-
    content(dataPage,
            type     = "text/csv",
            encoding = "UTF-8")
## Parsed with column specification:

content
httr:::parseability
httr:::parse_auto
httr:::parse_media
httr:::parsers

readr::read_csv

readr:::read_delimited
readr:::show_cols_spec

## readr:::read_delimited() throws "Parsed with column specification:" !!!
