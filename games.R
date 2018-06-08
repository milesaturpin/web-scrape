# scrape games data

library(rvest)
library(tidyverse)
library(stringr)

pages <- c("http://www.metacritic.com/browse/games/score/metascore/year/all/filtered?page=0",
           "http://www.metacritic.com/browse/games/score/metascore/year/all/filtered?page=1",
           "http://www.metacritic.com/browse/games/score/metascore/year/all/filtered?page=2")

paths <- c("#main .positive",
           ".small",
           ".small")

get_games <- function(url, path){
  
  page <- read_html(url)
  titles <- page %>%
    html_nodes("#main .product_title a") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    gsub("\\s+"," ", .) %>% 
    str_trim()
  
  metacritic_scores <- page %>% 
    html_nodes(path) %>% 
    html_text() %>% 
    as.numeric()
  
  user_scores <- page %>% 
    html_nodes("#main .textscore") %>% 
    html_text() %>% 
    as.numeric()
  
  release_dates <- page %>% 
    html_nodes(".product_date") %>% 
    html_text() %>% 
    str_remove_all("\n") %>% 
    str_trim()
  
  urls <- page %>% 
    html_nodes("#main .product_title a") %>% 
    html_attr("href") %>% 
    paste("http://www.metacritic.com", ., sep = "")
  
  games_indiv <- tibble(
    title = titles,
    metacritic_score = metacritic_scores,
    user_score = user_scores,
    release_date = release_dates,
    url = urls
  )
}

got_games <- map2_df(pages, paths, get_games)

# scrape individual game information
scrape_games <- function(url_temp){
  Sys.sleep(1)
  page <- read_html(url_temp)
  num_meta_temp <- page %>% 
    html_node(".count a span") %>% 
    html_text() %>% 
    as.numeric()
  num_user_temp <- page %>% 
    html_node(".feature_userscore .count a") %>% 
    html_text() %>% 
    str_remove("Ratings") %>%
    as.numeric()
  genres_temp <- page %>% 
    html_node(".product_genre") %>% 
    html_text() %>% 
    str_remove("Genre\\(s\\)\\:") %>% 
    gsub("\\s+"," ", .) %>% 
    str_trim()
  rating_temp <- page %>% 
    html_node(".product_rating .data") %>% 
    html_text() 
  tibble(
    num_meta = num_meta_temp,
    num_user = num_user_temp,
    genres = genres_temp, 
    rating = rating_temp,
    url = url_temp
  )
}

# get info for second game
scrape_games("http://www.metacritic.com/game/xbox-one/celeste")

# get info for all games released in 2018
urls_2018 <- got_games %>% 
  filter(!is.na(user_score)) %>% 
  select(url) %>%
  pull()

# scrape all urls; skip redundant rows
games <- map_df(urls_2018, scrape_games) %>% 
  right_join(got_games, by = "url")

colnames(games)[[6]] <- "game_title"

write_csv(games, "data/games.csv")



