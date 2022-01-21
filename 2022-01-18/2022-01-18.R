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
get_choc_sample <- slice_sample(choc_dat_sample, n = 20)

# Step 2 - Create Grid
choc_grid <- get_choc_sample %>%
  arrange(-rating) %>%
  mutate(radius = 1)




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

dec_colors <- c(
  "#FCBDBD", # light pink - Africa
  "#F66293", # dark pink - Americas
  "#A1A2E6", # purple - Asia
  "#91E7EB", # blue - Europe
  "#B1FDBD" # green - Oceania
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
