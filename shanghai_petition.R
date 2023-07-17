library(tidyverse)
library(jsonlite)
library(rvest)
library(httr2)
library(rio)

extract_info <- possibly(
    \(page) {
        link <- str_c("https://zfwzzc.www.gov.cn/check_web/errorInfo_querySearch.action?term=&searchType=-1&pageNo=", page, "&pos=3&size=10&problemIdbg=-1")

        info <- link %>%
            request() %>%
            req_timeout(12) %>%
            req_retry(
                max_tries = 5,
                max_seconds = 60,
                backoff = ~6
            ) %>%
            req_perform() %>%
            .$body %>%
            read_html() %>%
            html_text()
        
        Sys.sleep(sample(1:2, 1))
        
        return(info)
    },
    otherwise = "error!"
)

total <- extract_info(1) %>% 
    fromJSON() %>% 
    pluck("iTotalDisplayRecords")

info <- map(1:ceiling(total / 10), extract_info, .progress = TRUE)

table <- info[info != "error!"] %>% 
    map_dfr(\(x) fromJSON(x) |> pluck("body"))

export(
      table, 
      file = sprintf("data/table/%s_table.csv", Sys.Date()),
      bom = TRUE
    ) 
