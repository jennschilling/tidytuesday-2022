# Author : Jenn Schilling
# Title: #TidyTuesday Monthly Retail Sales
# Date: Dec 13 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(showtext)
library(lubridate)

#### Get the Data ####

state_retail <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")
coverage_codes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

# Add month name, month year combination, and turn change_yoy into a number
state_retail <-  state_retail |>
  mutate(month_name = month.abb[month],
         month_year = my(paste(month_name, year)),
         change_yoy_num = parse_number(change_yoy))

# Annotations
line_labels <- tibble(
  month = c(1.25, 11.85, 4, 3.5, 3, 8),
  change_yoy_num = c(13.4, -0.6, -87.2, 21, -10, 9.9),
  year = c(2019, 2019, 2020, 2020, 2019, 2022),
  hjust = c(0, 0.5, 0, 0, 0, 0),
  vjust = c(0.5, 1, 1, 0, 1, 0.5),
  subsector = c("Building Materials and Supplies Dealers",
                "Furniture and Home Furnishing",
                "Clothing and Clothing Accessories",
                "Food and Beverage",
                "Sporting Goods and Hobby",
                "Motor vehicle and parts dealers"),
  label = c("Building Materials and Supplies Dealers",
            "Furniture and\nHome Furnishing",
            "  Clothing and Clothing Accessories",
            "Food and Beverage",
            "  Sporting Goods and Hobby",
            " Motor vehicle and parts dealers"))

#### Formatting ####

# Font
font_add_google(name = "Lato")
showtext_auto()

# Theme Setup
font <- "Lato"
title_font <- "Lato"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#E4E4E4"

#### Plot ####

ggplot(data = state_retail |> 
         filter(state_abbr == "USA" &
                  naics %in% c(#441, # Motor vehicles and parts dealers
                               442, # Furniture and Home Furnishings
                               444, # Building Materials and Supplies Dealers
                               445, # Food and Beverage
                               448, # Clothing and Clothing Accessories
                               451 # Sporting Goods and Hobby
                               )),
       mapping = aes(x = month,
                     y = change_yoy_num,
                     color = subsector)) +
  geom_line(size = 0.5) +
  geom_text(data = line_labels |> filter(subsector != "Motor vehicle and parts dealers"),
            mapping = aes(label = label,
                          hjust = hjust,
                          vjust = vjust),
            family = font,
            size = 5,
            lineheight = 0.3) +
  facet_wrap(~ year,
             ncol = 1,
             scales = "free_y") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = 0.07)) +
  scale_x_continuous(breaks = seq(1, 12, 1),
                     labels = month.abb,
                     expand = expansion(mult = 0.02)) +
  scale_color_manual(values = c("#5f86b7", "#0e503e", "#42a359", 
                                "#97127b", "#8184fb", "#533f5e")) +
  guides(color = "none") +
  coord_cartesian(clip = "off") +
  labs(title = "Building materials and supplies along with food and beverage sales were not hit as\nhard by the pandemic and experienced positive year-over-year growth during 2020.\nClothing and clothing accessories along with furniture and home furnishings took a\nbig hit during 2020. Sporting goods and hobby year-over-year sales declined at the\nstart of the pandemic and then increased.",
       subtitle = "Lines show monthly year-over-year change in retail sales by industry for the United States.",
       x = "",
       y = "Year-over-Year Change",
       caption = "Data: United States Census Bureau's Monthly State Retail Sales | Design: Jenn Schilling") +
  theme(panel.background = element_rect(fill = bcolor, color = NA),
        plot.background = element_rect(fill = bcolor, color = NA),
        strip.background = element_rect(fill = bcolor, color = NA),
        
        text = element_text(family = font, color = font_color),
        
        axis.ticks = element_line(color = "white", size = 0.1),
        panel.grid.major = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        
        axis.title = element_text(family = font, color = font_color, size = 15),
        axis.text = element_text(family = font, color = font_color, size = 13),
        
        strip.text = element_text(family = font, color = font_color, 
                                  size = 20, hjust = 0),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(family = title_font, color = font_color,
                                  size = 25, lineheight = 0.3),
        plot.subtitle = element_text(family = font, color = font_color, size = 19),
        plot.caption = element_text(family = font, color = font_color, size = 14),
        
        plot.margin = margin(10, 10, 10, 10))

  

  


#### Save ####

ggsave(here("2022-12-13", "state_retail.png"),
       plot = last_plot(),
       device = "png",
       width = 4.1,
       height = 4.5)


