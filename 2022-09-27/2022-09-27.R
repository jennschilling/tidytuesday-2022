# Author : Jenn Schilling
# Title: #TidyTuesday US Artists
# Date: Sep 27 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(geofacet)
library(treemapify)


#### Get the Data ####

artists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

# Get just the writers and authors and compute percentages for each race/ethnicity in each state
writers_authors <- artists %>%
  filter(type == "Writers And Authors") %>%
  group_by(state) %>%
  mutate(total_artists = sum(artists_n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_artists = artists_n / total_artists)

# Get the total race/ethnicity breakdown for the country
total_race_eth <- writers_authors %>%
  group_by(race) %>%
  summarise(artists_n = sum(artists_n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_artists = sum(artists_n)) %>%
  mutate(prop_artists = artists_n / total_artists)


#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
title_font <- "Montserrat"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#e9eff4"

#### Plot ####

ggplot(data = writers_authors,
       mapping = aes(area = prop_artists,
                     fill = race,
                     color = race)) +
  geom_treemap() +
  facet_geo(~ state,
            labeller = label_wrap_gen(width = 14)) +
  scale_fill_manual(values = c("#8C657B", "#B3948F", "#6089BF",
                               "#D9A566", "#808696")) +
  scale_color_manual(values = c("#8C657B", "#B3948F", "#6089BF",
                               "#D9A566", "#808696")) +
  guides(color = "none") +
  labs(title = "82.7% of writers and authors in the United States are white.",
       subtitle = "5.0% are African American; 3.7% are Asian; 5.7% are Hispanic; and 2.9% are of another race/ethnicity.\n",
       fill = "Race/Ethnicity",
       caption = "Data: arts.gov by way of Data is Plural | Design: Jenn Schilling") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = bcolor, colour = NA),
        strip.text = element_text(family = font, size = 18, 
                                  color = font_color, lineheight = 0.3, 
                                  vjust = 0, hjust = 0, 
                                  margin = margin(0.5, 0, 1, -0.2)),
        
        legend.position = "top",
        # legend.background = element_rect(fill = 'transparent', color = 'transparent'),
        # legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
        # legend.key = element_rect(fill = 'transparent', color = 'transparent'),
        legend.title = element_blank(),
        legend.text = element_text(family = font, size = 22, color = font_color),
        
        panel.background = element_rect(fill = bcolor, colour = NA),
        plot.background = element_rect(fill = bcolor, colour = NA),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 38, color = font_color, family = title_font, lineheight = 0.4),
        plot.subtitle = element_text(size = 30, color = font_color, family = title_font, lineheight = 0.4),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 14, color = font_color, lineheight = 0.3, hjust = 1, family = title_font),
       
        plot.margin = margin(10, 10, 10, 10))




#### Save ####

ggsave(here("2022-09-27", "us_writers_race.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo",
       width = 8.5,
       height = 5.5)



