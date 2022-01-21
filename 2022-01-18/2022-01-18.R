# Author : Jenn Schilling
# Title: #TidyTuesday Chocolate Ratings
# Date: Jan 18 2022

#### Libraries ####

library(tidyverse)
library(ggforce)
library(extrafont)
library(ggtext)
library(scales)
library(countrycode)

#### Get the Data ####

chocolate <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

choc_dat <- chocolate %>%
  mutate(cocoa_percent = parse_number(cocoa_percent),
         country_of_bean_origin = str_replace_all(country_of_bean_origin, "[.]", ""),
         company_location = str_replace_all(company_location, "[.]", ""))

# Cocoa percents with the most chocolates
# 70 - 1046
# 75 - 310
# 72 - 295
# 65 - 90
# 80 - 89
# 68 - 72
# 74 - 67
# 73 - 66

# 100% - 21
# Less than 60% 8 + 1 + 2+ 16 + 1 + 1 + 1 + 1 = 31
# More than 90% 3 + 2 + 21 = 26

# Ingredients with the most chocolates
# 3- B,S,C - 999
# 2- B,S - 718
# 4- B,S,C,L - 286
# 5- B,S,C,V,L - 184
# 4- B,S,C,V - 141

# Uses alternate sugar: 31 + 12 + 1 + 2 + 20 + 7 + 3 = 76

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
fontcolor <- "#FAFAFA"
bcolor <- "#3F3F3F"

choc_colors <- c(
  "#EEE1D0", # white chocolate
  "#A6705D", # light brown
  "#8C5946",
  "#593325",
  "#401E12",
  "#260F07" # dark brown
)

gold <- "#D9AE30"

rectangle_sides <- "#4F3D0F"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = gold, color = NA),
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

# Can't do this because there is more than 1 intersection at each combination
ggplot(data = chocolate) +
  geom_tile(mapping = aes(x = cocoa_percent,
                          y = ingredients,
                          fill = rating))
