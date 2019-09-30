library(tidyverse)
library(rvest)
library(viridis)
library(ggforce)
library(here)

# pull in webscraped data
# run R/webscrape-data-BeerAdvocate.R to update data
d <- readRDS(file = "./data/beeradvocate-top-250_webscraped_2019-09-29.rds")

# create style/family to srm mapping for plot colors
srm_rounding <- function(srm) {
  case_when(
    srm < 13 ~ round(srm*2, digits = 0)/2,
    srm >= 13 ~ round(srm, digits = 0)
  )
}

srm_to_hex <- 
  "https://brookstonbeerbulletin.com/thinking-about-beer-color/" %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  .[[4]] %>% 
  html_table() %>% 
  mutate(Code = paste0("#",Code)) %>% 
  as_tibble() %>% 
  mutate_at(vars(SRM), parse_number)

style_to_srm <- 
  readxl::read_xlsx("data/beer-style-family-srm.xlsx",
                    sheet = "style_to_srm") 

d <-
  d %>% 
  left_join(style_to_srm, by = c("style" = "style")) %>% 
  group_by(family) %>% 
  mutate(srm_family_avg = srm_rounding(mean(srm_avg))) %>%  # naturally weighted by size of style
  left_join(srm_to_hex %>% select("SRM","Code"), by = c("srm_family_avg" = "SRM"))

# srm_to_hex; style_to_srm; d

family_code_mapping <- d %>% select(family, Code) %>% unique() %>% deframe
family_order <- d %>% group_by(family) %>% tally(sort = TRUE) %>% pull(family)
style_order <- d %>% group_by(style) %>% tally(sort = TRUE) %>% pull(style)

library(treemapify)
# devtools::install_github("clauswilke/ggtext")
library(ggtext)

# build the main treemap plot
p_treemap <- 
  d %>% 
  group_by(style) %>%
  summarize(avg_score = mean(score),
            count = n(),
            Code = max(Code),
            family = max(family)) %>% 
  ggplot(aes(area = count, label = style, fill = family, subgroup = family)) +
  geom_treemap(start = "topleft") +
  geom_treemap_text(color = "white",
                    start = "topleft",
                    reflow = TRUE,
                    size = 10) +
  # geom_treemap_subgroup_border(color = "white", start = "topleft") +
  scale_fill_manual(values = family_code_mapping,
                    name = " ",
                    limits = family_order) + 
  # scale_fill_identity(guide = "legend") +
  theme() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0)) + 
  labs(title = "<b style='color:#2f0200'>Stouts</b> and <b style='color:#c86505'>IPAs</b> Dominate the Palettes of Beer Enthusiasts",
       subtitle = "Distribution of styles among BeerAdvocate's Top 250 Rated Beers",
       caption = "Size represents number of beers in BeerAdvocate's Top 250 Rated Beers.  Data retrieved from https://www.beeradvocate.com/beer/top-rated/ on 9/29/19.
Graphic created by @Sean__Kent.") +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1))


'%ni%' <- Negate('%in%')

# make bar chart legend 
p_family_legend <- 
  d %>% 
  ggplot(aes(x = family, fill = family)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label=family), 
            stat = "count",
            hjust=1.1,
            color="grey95",
            size = 3,
            # fontface="bold",
            data = d %>% filter(family %in% c("Stouts","India Pale Ales"))
  ) + 
  geom_text(aes(label=family), 
            stat = "count",
            hjust=-0.05,
            color="grey5",
            size = 3,
            # fontface="bold",
            data = d %>% filter(family %ni% c("Stouts","India Pale Ales"))
  ) + 
  scale_x_discrete(limits = rev(family_order),
                   breaks = NULL) +
  scale_fill_manual(values = family_code_mapping,
                    name = " ",
                    limits = NULL) +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "none") +
  ylim(c(0,100)) + 
  labs(x = "", y = "", subtitle = "Count by Beer Family") 


# produce the final plot with grid.arrange 
library(gridExtra)
grid.arrange(
  grobs = list(p_treemap, p_family_legend),
  layout_matrix = rbind(c(1,NA),
                        c(1,2),
                        c(1,NA)),
  widths = c(4.5,1),
  heights = c(1,4,1)
)
