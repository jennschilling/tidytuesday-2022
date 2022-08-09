# Author : Jenn Schilling
# Title: #TidyTuesday Ferris Wheels
# Date: Aug 9 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(lubridate)

#### Get the Data ####

wheels <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

wheels_p <- wheels %>%
  mutate(year = year(opened),
         status = ifelse(is.na(status), "Unknown Status", status),
         highest = height >= 600)

status_line <- wheels_p %>%
  filter(!is.na(height)) %>%
  group_by(status) %>%
  summarise(min_height = min(height),
            max_height = max(height)) %>%
  ungroup() %>%
  mutate(min_height = -200,
         max_height = max_height + 30) %>%
  arrange(desc(status)) %>%
  mutate(status_index = row_number())


#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#e9eff4"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 28, color = font_color),
  axis.text = element_text(size = 26, color = font_color),
  axis.ticks = element_line(color = line_color),
  axis.line = element_line(color = line_color),
  
  strip.text = element_text(size = 26, color = font_color, hjust = 0),
  
  legend.text = element_text(size = 26, color = font_color),
  legend.title = element_text(size = 26, color = font_color),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 60, color = font_color, lineheight = 0.3, hjust = 0.5),
  
  plot.subtitle = element_text(size = 26, color = font_color, lineheight = 0.3),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 14, color = font_color, hjust = 1)
)


#### Explore Data ####



#### Plot ####

ggplot() +
  geom_point(data = wheels_p %>% filter(!is.na(height)),
             mapping = aes(x = height,
                           y = fct_rev(status),
                           color = highest),
             size = 7,
             alpha = 0.5) +
  geom_segment(data = status_line,
               mapping = aes(x = min_height,
                             xend = max_height,
                             y = status_index - 0.28,
                             yend = status_index - 0.28),
               color = font_color,
               size = 0.5) +
  geom_text(data = status_line,
            mapping = aes(x = min_height,
                          y = status_index,
                          label = status),
            color = font_color,
            family = font,
            size = 15,
            hjust = 0) +
  guides(color = "none") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-200, 900),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = comma) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("#53868b", "#f18805")) +
  labs(x = "Height (ft)",
       title = "The tallest Ferris wheels are still being built.",
       subtitle =  "",
       caption = "\nData {ferriswheels} by Emil Hvitfeldt | Design Jenn Schilling") +
  theme(plot.margin = margin(25, 25, 25, 25),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())


#### Save ####

ggsave(here("2022-08-09", "ferris.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 6,
       height = 6)  

