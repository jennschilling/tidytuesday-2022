# Author : Jenn Schilling
# Title: #TidyTuesday Board Games
# Date: Jan 25 2022

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Get the Data ####

ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

#### Process the Data ####

games <- left_join(ratings, details, by = "id") %>%
  mutate(category = str_split(boardgamecategory, ",")) %>%
  unnest(category) %>%
  mutate(category = str_replace_all(category, "\\'|\\[|\\]", ""),
         category = str_replace_all(category, '\\"', ""),
         category = str_squish(category))

top_categories <- games %>%
  count(category) %>%
  slice_max(order_by = n, n = 10) %>%
  select(-n)

top_cat_games <- left_join(top_categories, games, by = "category")

#### Formatting ####

font <- "Candara"
title_font <- "Candara"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

ggplot(data = top_cat_games) +
  geom_point(mapping = aes(x = category,
                           y = bayes_average,
                           color = minplayers),
             position = position_jitter(),
             alpha = 0.5)
