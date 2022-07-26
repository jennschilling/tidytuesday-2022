# Author : Jenn Schilling
# Title: #TidyTuesday Tuskegee Airmen
# Date: Feb 8 2022

#### Libraries ####

library(tidyverse)
library(ggforce)
library(extrafont)
library(ggtext)

#### Get the Data ####

airmen <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

airmen <- airmen %>%
  mutate(state = str_to_upper(state),
         state = ifelse(is.na(state), "UNK", state))

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
fontcolor <- "gray30"
bcolor <- "white"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title.y = element_text(size = 12, color = fontcolor, angle = 0, vjust = 0.98),
  axis.title.x = element_blank(),
  axis.text = element_text(size = 12, color = fontcolor),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 20, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 12, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####