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


#### Process the Data ####

choc_dat <- chocolate %>%
  mutate(cocoa_percent = parse_number(cocoa_percent),
         num_ingredients = parse_number(ingredients),
         country_of_bean_origin = str_replace_all(country_of_bean_origin, "[.]", ""),
         country_of_bean_origin = case_when(
           country_of_bean_origin == "Burma" ~ "Myanmar (Burma)", 
           country_of_bean_origin == "Congo" | country_of_bean_origin == "DR Congo" ~ "Congo - Kinshasa",
           country_of_bean_origin == "Ivory Coast" ~ "Côte d’Ivoire",
           country_of_bean_origin == "Principe" | country_of_bean_origin == "Sao Tome" | 
             country_of_bean_origin == "Sao Tome & Principe" ~ "São Tomé & Príncipe",
           country_of_bean_origin == "St Lucia" ~ "St. Lucia",
           country_of_bean_origin == "StVincent-Grenadines" ~ "St. Vincent & Grenadines",
           country_of_bean_origin == "Trinidad" | country_of_bean_origin == "Tobago" ~ "Trinidad & Tobago",
           country_of_bean_origin == "USA" ~ "United States",
           TRUE ~ country_of_bean_origin),
         company_location = str_replace_all(company_location, "[.]", ""),
         company_location = case_when(
           company_location == "Burma" ~ "Myanmar (Burma)", 
           company_location == "Congo" | company_location == "DR Congo" ~ "Congo - Kinshasa",
           company_location == "Ivory Coast" ~ "Côte d’Ivoire",
           company_location == "Principe" | company_location == "Sao Tome" | 
             company_location == "Sao Tome & Principe" ~ "São Tomé & Príncipe",
           company_location == "St Lucia" ~ "St. Lucia",
           company_location == "StVincent-Grenadines" ~ "St. Vincent & Grenadines",
           company_location == "Trinidad" | company_location == "Tobago" ~ "Trinidad & Tobago",
           company_location == "USA" ~ "United States",
           company_location == "UK" | company_location == "Scotland" |
             company_location == "Wales" ~ "United Kingdom",
           company_location == "UAE" ~ "United Arab Emirates",
           company_location == "Amsterdam" ~ "Netherlands",
           company_location == "Czech Republic" ~ "Czechia",
           TRUE ~ company_location)) %>%
  left_join(codelist %>% select(country.name.en, continent, region), by = c("country_of_bean_origin" = "country.name.en")) %>%
  rename(cont_of_bean_origin = continent,
         reg_of_bean_origin = region) %>%
  left_join(codelist %>% select(country.name.en, continent, region), by = c("company_location" = "country.name.en")) %>%
  rename(cont_company_location = continent,
         reg_company_location = region) 

#### Formatting ####

font <- "Candara"
title_font <- "Candara"
font_color <- "#FAFAFA"
b_color <- "#FFFEF2"

choc_colors <- c(
#  "#EEE1D0", # white chocolate
  "#A6705D", # light brown
  "#8C5946",
  "#593325",
  "#401E12",
  "#260F07" # dark brown
)

dec_colors <- c(
  "#FCBDBD", # light pink - Africa
  "#F66293", # dark pink - Americas
  "#A1A2E6", # purple - Asia
  "#91E7EB", # blue - Europe
  "#B1FDBD" # green - Oceania
)

panel_col <- "#BFA300"

rectangle_sides <- "#4F3D0F"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = panel_col, color = rectangle_sides, size = 4),
  plot.background = element_rect(fill = b_color, color = NA),
  
  axis.title.x = element_text(size = 10, color = rectangle_sides, hjust = 1, face = "italic"),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 10, color = rectangle_sides, hjust = 1, angle = 0),
  axis.ticks = element_blank(),
  axis.line = element_line(color = rectangle_sides, size = 2),
  
  strip.text = element_text(size = 10, color = font_color, hjust = 0),
  
  legend.text = element_text(size = 10, color = font_color),
  legend.title = element_text(size = 10, color = font_color),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 16, color = rectangle_sides, family = title_font),
  
  plot.subtitle = element_markdown(size = 10, color = rectangle_sides),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 9, color = rectangle_sides),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Make Grid ####

## Plan ##
# 1. Sample 20 chocolates - removing NAs from continent or region of company/bean location, number of ingredients, cocoa percent, rating
# 2. Arrange chocolates from highest rated to lowest rated in a grid
# 3. Fill shape by cocoa percent
# 4. Type of shape by continent of bean location (number of sides)
# 5. Color of decoration by continent of company 
# 6. Number of decorations by number of ingredients

choc_dat_sample <- choc_dat %>%
  select(cont_company_location, cont_of_bean_origin, num_ingredients, cocoa_percent, rating) %>%
  filter(complete.cases(.))

# Step 1 - Sample
sample_size <- 20

get_choc_sample <- slice_sample(choc_dat_sample, n = sample_size)

# Step 2 - Create Grid
choc_grid <- get_choc_sample %>%
  arrange(rating) 

make_grid <- function(length, width, radius){

  x <- seq(from = 2, to = width*2, by = 2) 
  y <- seq(from = 2, to = length*2, by = 2) 

  coords <- expand_grid(x, y)

  r <- rep(radius, nrow(coords))

  grid <- bind_cols(coords, r = r)
  
  return(grid)

}

width <- 5
length <- 4
radius <- 0.8

coord_grid <- make_grid(length, width, radius)

choc_coord_grid <- bind_cols(choc_grid, coord_grid) %>%
  arrange(-cocoa_percent) %>%
  mutate(percent_fill = colorRampPalette(choc_colors)(sample_size),
         sides = case_when(
           cont_of_bean_origin == "Africa" ~ 3,
           cont_of_bean_origin == "Asia" ~ 4,
           cont_of_bean_origin == "Ocenia" ~ 5,
           TRUE ~ 0
         ),
         angle = 0)

box_radius <- 1.5

box_grid <- make_grid(length, width, box_radius) %>%
  mutate(sides = 4,
         angle = 0)


#### Plot ####

ggplot() +
  # Square background
  geom_regon(data = box_grid,
             mapping = aes(x0 = x,
                           y0 = y,
                           r = r,
                           sides = sides,
                           angle = angle),
             fill = b_color,
             color = NA) +
  # Square border
  geom_regon(data = box_grid,
             mapping = aes(x0 = x,
                           y0 = y,
                           r = r,
                           sides = sides,
                           angle = angle),
             fill = NA,
             color = rectangle_sides,
             size = 2) +
  # Chocolate shapes
  geom_circle(data = choc_coord_grid %>% filter(cont_of_bean_origin == "Americas"),
              mapping = aes(x0 = x,
                            y0 = y,
                            r = r,
                            fill = percent_fill),
              color = NA) +
  geom_regon(data = choc_coord_grid %>% filter(cont_of_bean_origin != "Americas"),
             mapping = aes(x0 = x,
                           y0 = y,
                           r = r,
                           fill = percent_fill,
                           sides = sides,
                           angle = angle),
             color = NA) +
  scale_fill_identity() +
  scale_y_continuous(breaks = c(1, 9),
                     labels = c("Low\nRating", "High\nRating")) +
  labs(title = "Life is like a box of chocolates...you never know what you're going to get.<br>",
       x = "Sample of 20 chocolates",
       caption = "<br>Data: <b>Flavors of Cacao</b> | Design: <b>Jenn Schilling</b>")
