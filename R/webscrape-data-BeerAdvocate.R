library(tidyverse)
library(rvest)
library(here)

# Webscrape data 
extract_abv <- function(detail_string) {
  # "Toppling Goliath Brewing CompanyAmerican Imperial Stout | 12.00%"
  # goes to 12.00
  detail_string %>% 
    str_split(fixed("|")) %>% 
    map(., ~.x[2]) %>% 
    unlist() %>% 
    str_remove("%") %>% 
    as.numeric()
}

url <- "https://www.beeradvocate.com/beer/top-rated/"

css_tags <- c(
  ".hr_bottom_light a b", # beer name
  ".hr_bottom_light:nth-child(3) b", # n_ratings
  ".hr_bottom_light:nth-child(4) b", # average of ratings
  ".muted a~ a", # beer type
  ".hr_bottom_light .muted" # brewery, alcohol content, and beer type
)

d <-
  css_tags %>% 
  map(~ html_text(html_nodes(read_html(url), .))) %>% 
  transpose() %>% 
  {tibble(beer_name = map(., 1),
          n_ratings = map(., 2),
          score = map(., 3),
          style = map(., 4),
          details = map(., 5)) } %>% 
  unnest(cols = c(beer_name, n_ratings, score, style, details)) %>% 
  mutate(
    n_ratings = as.numeric(gsub(",", "", n_ratings)),
    score = as.numeric(score),
    abv = extract_abv(details)
  )

# save the data at this date
filename <- paste0("beeradvocate-top-250_webscraped_",lubridate::today(),".rds") 
saveRDS(d, file = here("data",filename))
