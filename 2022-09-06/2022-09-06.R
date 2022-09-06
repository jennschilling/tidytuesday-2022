# Author : Jenn Schilling
# Title: #TidyTuesday LEGO Sets
# Date: Sep 6 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)

#### Get the Data ####

inventories <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_parts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
colors <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
sets <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')


lego_data <- left_join(inventories, sets, by = "set_num") %>%
  left_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  left_join(colors, by = c("color_id" = "id"))

lego_data_agg <- lego_data %>%
  filter(!is.na(name.y) & !is.na(year)) %>%
  group_by(year, name.y, rgb) %>%
  summarize(n = sum(quantity)) %>%
  ungroup() %>%
  rename(color_name = name.y) %>%
  group_by(year) %>%
  mutate(perc = n / sum(n),
         num_colors = n()) %>%
  ungroup() %>%
  mutate(rgb = paste("#", rgb, sep = ""))

#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#e9eff4"


#### Plot ####

ggplot(data = lego_data_agg,
       mapping = aes(x = year,
                     y = perc,
                     fill = rgb)) +
  geom_col() +
  annotate("text",
           x = 1949,
           y = 1.025,
           label = "Percent\nof Parts",
           size = 4.5,
           family = font,
           color = font_color,
           hjust = 1.4,
           vjust = -0.1,
           lineheight = 0.3) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_fill_identity() +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "Over time, the number of LEGO colors has increased.",
       subtitle = "In the early years, parts of only 3-10 colors were used. In recent years, parts of 65-70 colors are used.\n\n",
       x = "Year",
       caption = "\nBased on the release year of sets and the number of parts of each color used in each set.\nData: rebrickable | Design: Jenn Schilling") +
  theme(axis.title.x = element_text(size = 14, color = font_color, family = font),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12, color = font_color, family = font),
        axis.ticks = element_line(color = line_color, size = 0.1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = bcolor),
        plot.background = element_rect(fill = bcolor),
        plot.title.position = "plot",
        plot.title = element_text(size = 40, color = font_color, lineheight = 0.3, family = font),
        plot.subtitle = element_text(size = 20, color = font_color, lineheight = 0.3, family = font),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 14, color = font_color, lineheight = 0.3, hjust = 1, family = font),
        plot.margin = margin(10, 10, 10, 10))


#### Save ####

ggsave(here("2022-09-06", "lego.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 5,
       height = 3)  
