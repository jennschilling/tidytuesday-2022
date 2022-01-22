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
library(patchwork)

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
  
  axis.title.x = element_text(size = 14, color = rectangle_sides, hjust = 1, face = "italic"),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), #element_text(size = 12, color = rectangle_sides, hjust = 1, angle = 0),
  axis.ticks = element_blank(),
  axis.line = element_line(color = rectangle_sides, size = 2),
  
  strip.text = element_text(size = 10, color = font_color, hjust = 0),
  
  legend.text = element_text(size = 10, color = font_color),
  legend.title = element_text(size = 10, color = font_color),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 24, color = rectangle_sides, family = title_font),
  
  plot.subtitle = element_markdown(size = 10, color = rectangle_sides),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 12, color = rectangle_sides),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Functions ####

# Decoration Data Function
make_decor_lines <- function(choc_coord_grid){
  x <- vector("numeric")
  xend <- vector("numeric")
  y <- vector("numeric")
  yend <- vector("numeric")
  color <- vector("character")
  j <- 1
  
  for(i in 1:sample_size){
    
    cont <- choc_coord_grid[i,]$cont_of_bean_origin
    num_ing <- choc_coord_grid[i,]$num_ingredients
    x0 <- choc_coord_grid[i,]$x
    y0 <- choc_coord_grid[i,]$y
    r <- choc_coord_grid[i,]$r
    
    cont_color <- choc_coord_grid[i,]$cont_company_location
    conts <- list("Africa", "Americas", "Asia", "Europe", "Oceania")
    col_index <- which(conts == cont_color)
    line_color <- dec_colors[col_index]
    
    if(cont == "Americas"){# circle
      
      theta <- 10
      
      for(k in 1:num_ing){
        
        x[j] <- x0 + r * sin(theta)
        xend[j] <- x0 + r * sin(180 + theta)
        y[j] <- y0 + r * cos(theta)
        yend[j] <- y0 + r * cos(180 + theta)
        color[j] <- line_color
        
        j <- j + 1
        theta <- theta + 1
        
      }
      
    }else if(cont == "Africa"){ # triangle
      
      decrease <- 0
      
      for(k in 1:num_ing){
        
        x[j] <- x0 - decrease
        xend[j] <- x0 - decrease
        y[j] <- y0 + r * (1 - 0.06 * k) - decrease
        yend[j] <- y0 - r/2 * (1.25 - 0.25 * k) - decrease
        color[j] <- line_color
        
        j <- j + 1
        decrease <- decrease + 0.1
        
      }
      
    }else if(cont == "Asia"){ # square
      
      decrease <- 0
      
      for(k in 1:num_ing){
        
        x[j] <- x0 - r * 0.75
        xend[j] <- x0 + r * 0.75
        y[j] <- y0 - decrease
        yend[j] <- y0 - decrease
        color[j] <- line_color
        
        j <- j + 1
        decrease <- decrease + 0.1
        
      }
      
    }else if(cont == "Oceania"){ # pentagon
      
      decrease <- 0
      
      for(k in 1:num_ing){
        
        x[j] <- x0 - r * (1 - k * 0.08)  - decrease
        xend[j] <- x0 + r * (1 - k * 0.04) - decrease 
        y[j] <- y0 - decrease * 5
        yend[j] <- y0 - decrease * 5
        color[j] <- line_color
        
        j <- j + 1
        decrease <- decrease + 0.02
        
      }
    }
  }
  
  decor_lines <- bind_cols(x = x, 
                           xend = xend, 
                           y = y, 
                           yend = yend, 
                           color = color)
  
  return(decor_lines)
  
}

# Grid Function
make_grid <- function(length, width, radius){
  
  x <- seq(from = 2, to = width*2, by = 2) 
  y <- seq(from = 2, to = length*2, by = 2) 
  
  coords <- expand_grid(x, y)
  
  r <- rep(radius, nrow(coords))
  
  grid <- bind_cols(coords, r = r)
  
  return(grid)
  
}


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

#### Step 1 - Sample ####
sample_size <- 20
num <- 11

get_choc_sample <- slice_sample(choc_dat_sample, n = sample_size)

#### Step 2 - Create Grid ####
choc_grid <- get_choc_sample %>%
  arrange(rating) 

# Chocolates Data
width <- 5
length <- 4
radius <- 0.8

coord_grid <- make_grid(length, width, radius)

#### Step 3 & 4 - Set Fill and Shape ####
choc_coord_grid <- bind_cols(choc_grid, coord_grid) %>%
  arrange(cocoa_percent) %>%
  mutate(percent_fill = colorRampPalette(choc_colors)(sample_size),
         sides = case_when(
           cont_of_bean_origin == "Africa" ~ 3,
           cont_of_bean_origin == "Asia" ~ 4,
           cont_of_bean_origin == "Oceania" ~ 5,
           TRUE ~ 0
         ),
         angle = 0)

# Box Data
box_radius <- 1.5

box_grid <- make_grid(length, width, box_radius) %>%
  mutate(sides = 4,
         angle = 0)

#### Step 5 & 6 - Make Decorations ####
decor_lines <- make_decor_lines(choc_coord_grid)


#### Legend Data ####
bean_origin <- bind_cols(make_grid(4, 1, 1), 
                         sides = c(5, 4, 3, 1), 
                         label = c("Oceania", "Asia", "Africa", "Americas"))
company_loc <- bind_cols(make_grid(5, 1, 1), 
                         color = rev(c("#F66293", "#FCBDBD", "#A1A2E6", "#91E7EB", "#B1FDBD")),
                         label = rev(c("Americas", "Africa", "Asia", "Europe", "Oceania")))
  
#### Plot ####

# MAIN PLOT #

plot <- ggplot() +
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
                           r = r * 1.1,
                           fill = percent_fill,
                           sides = sides,
                           angle = angle),
             color = NA) +
  # Chocolate Decorations
  geom_segment(data = decor_lines,
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend,
                             color = color),
               size = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(breaks = c(1, 9),
                     labels = c("Low\nRating", "High\nRating")) +
  labs(title = "",
       x = "An assortment of 20 chocolates from the #TidyTuesday dataset.")

# LEGEND #

plot_legend <- ggplot() +
  geom_circle(data = bean_origin %>% filter(sides == 1),
              mapping = aes(x0 = x / 2,
                            y0 = y / 2,
                            r = r / 2),
              fill = rectangle_sides,
              color = NA) +
  geom_regon(data = bean_origin %>% filter(sides > 1),
             mapping = aes(x0 = x / 2,
                           y0 = y / 2,
                           r = r / 2,
                           sides = sides,
                           angle = 0),
             fill = rectangle_sides,
             color = NA) +
  geom_text(data = bean_origin,
            mapping = aes(x = x / 2 + r,
                          y = y / 2,
                          label = label),
            family = font,
            color = rectangle_sides,
            size = 5,
            hjust = 0) +
  geom_segment(data = company_loc,
               mapping = aes(x = x + 3 * r,
                             xend = x + 4 * r,
                             y = y / 2.5,
                             yend = y / 2.5,
                             color = color),
               size = 2) +
  geom_text(data = company_loc,
            mapping = aes(x = x + 4.5 * r,
                          y = y / 2.5,
                          label = label),
            family = font,
            color = rectangle_sides,
            size = 5,
            hjust = 0) +
  annotate("text",
           x = 0.5,
           y = -0.7,
           label = "The number of lines indicates the number of\ningredients.",
           size = 5,
           family = font,
           color = rectangle_sides,
           hjust = 0,
           lineheight = 1) +
  annotate("text",
           x = 0.5,
           y = -2,
           label = "The color of the chocolate indicates the cocoa \npercent (darker color = higher percent chocolate).",
           size = 5,
           family = font,
           color = rectangle_sides,
           hjust = 0,
           lineheight = 1) +
  annotate("text",
           x = 0.5,
           y = -3.5,
           label = "Placement indicates the rating - lowest rating in\nthe bottom left corner, highest rating in the\ntop right corner.",
           size = 5,
           family = font,
           color = rectangle_sides,
           hjust = 0,
           lineheight = 1) +
  annotate("text",
           x = 0.5,
           y = 6,
           label = "How to read the chocolates:",
           size = 6,
           family = font,
           color = rectangle_sides,
           hjust = 0,
           fontface = "italic") +
  annotate("text",
           x = 0.5,
           y = 5,
           label = "Bean Source:",
           size = 5,
           family = font,
           color = rectangle_sides,
           hjust = 0) +
  
  annotate("text",
           x = 5,
           y = 5,
           label = "Manufacturer Location:",
           size = 5,
           family = font,
           color = rectangle_sides,
           hjust = 0) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  labs(title = "") +
  scale_x_continuous(limits = c(0.5, 10)) +
  scale_y_continuous(limits = c(-10, 6)) +
  theme(panel.background = element_rect(fill = b_color, color = NA),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        plot.subtitle = element_markdown(size = 16, color = rectangle_sides))

## Put Plots Together ##

plot + plot_legend +
  plot_layout(widths = c(3, 1)) +
  plot_annotation(title = "Life is like a box of chocolates...you never know what you're going to get.<br>",
                  caption = "<br>Data: <b>Flavors of Cacao</b> | Design: <b>Jenn Schilling</b>")

ggsave(paste("2022-01-18\\choc_box_", num, ".png", sep = ""),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 18,
       height = 10,
       dpi = 500)


