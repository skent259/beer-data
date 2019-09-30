library(tidyverse)
library(rvest)
library(viridis)

## Webscrape data 
url <- "https://www.beeradvocate.com/lists/top/"

css_tags <- c(".hr_bottom_light a b", ".hr_bottom_light:nth-child(3) b",
              ".hr_bottom_light:nth-child(4) b", "#extendedInfo a:nth-child(1)",
              "#extendedInfo br+ a")

data <- css_tags %>%
  map(~ html_text(html_nodes(read_html(url), .))) %>%
  transpose() %>%
  {tibble(beer_name = map(., 1),
          score = map(., 2),
          n_ratings = map(., 3),
          brewery = map(., 4),
          style = map(., 5)) } %>%
  unnest() %>%
  mutate(n_ratings = gsub(",", "", n_ratings)) %>%
  mutate_at(vars(score, n_ratings), as.numeric)

## exploratory data analysis

unique(data %>% pull(style))

top_styles <- data %>% 
  group_by(style) %>% 
  summarise(count = n()) %>% 
  filter(count > 10) %>% 
  arrange(-count) %>%
  pull(style)

top_breweries <- data %>% 
  group_by(brewery) %>% 
  summarise(count = n()) %>% 
  filter(count > 5) %>% 
  arrange(-count) %>%
  pull(brewery)

data %>% 
  group_by(brewery) %>% 
  summarise(count = n()) %>%
  arrange(-count)

data %>% filter(style == "Saison / Farmhouse Ale")

## make some graphs!

## Boxplot of top styles vs ratings
data %>% filter(style %in% top_styles) %>%
  ggplot(aes(x=style, y=score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = style), width = 0.25) +
  scale_x_discrete(limits = top_styles) +
  theme(axis.text.x = element_text(angle = 340, hjust = 0.1))

data %>% filter(brewery %in% top_breweries) %>%
  ggplot(aes(x=brewery, y=score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = brewery), width = 0.2, size = 2) +
  scale_x_discrete(limits = top_breweries) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 340, hjust = 0.05),
        legend.position = "none")

data %>% filter(brewery %in% top_breweries) %>%
  ggplot(aes(x=brewery, y=score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = style), width = 0.2, size = 2) +
  scale_x_discrete(limits = top_breweries) +
  scale_color_discrete() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 340, hjust = 0.05))


## Barplot of popular styles with coloring/labels for score of top rated beer
data %>% 
  group_by(style) %>%
  mutate(avg_score = max(score)) %>%
  ungroup() %>%
  ggplot(aes(x = style,
             fill = avg_score)) + 
  geom_bar(stat = "count" ) + 
  geom_text(stat = "count",
            aes(label= data %>%
                  group_by(style) %>%
                  mutate(avg_score = max(score)) %>%
                  pull(avg_score) %>%
                  formatC(2, format = "f") ),
            hjust=-0.10, color="black", fontface="italic") +
  scale_x_discrete(limits = data %>% 
                     group_by(style) %>% 
                     summarise(count = n()) %>% 
                     arrange(count) %>%
                     pull(style)
  ) +
  coord_flip() +
  scale_fill_gradient(low = "#f0c566", high = "#882300", name = "Score") +
  labs(title = "Popular Styles Among Top 250 Beers",
       subtitle = "Top Beers According to BeerAdvocate",
       x = "", y = "Count",
       caption = "Labels show BeerAdvocate score (0-5) of highest rated beer in each category.  Data retrieved from https://www.beeradvocate.com/lists/top/ 
on 7/16/18.  Graphic created by @Sean__Kent.") +
  theme(plot.caption=element_text(size=10, hjust=0, margin=margin(t=15)))
