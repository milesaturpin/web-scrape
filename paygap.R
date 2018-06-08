# scrape paygap data

library(rvest)
library(tidyverse)

page <- read_html("https://en.wikipedia.org/wiki/Gender_pay_gap")

countries <- page %>% 
  html_nodes("td:nth-child(1)") %>% 
  html_text() %>% 
  .[11:46]

pay_gaps <- page %>% 
  html_nodes("td:nth-child(2)") %>% 
  html_text() %>% 
  .[1:36] %>% 
  as.numeric()

years <- page %>% 
  html_nodes("td~ td+ td") %>% 
  html_text() %>% 
  as.numeric() 

paygap <- tibble(
  country = countries,
  pay_gap = pay_gaps,
  year = years
)

write_csv(paygap, path = "data/paygap.csv")