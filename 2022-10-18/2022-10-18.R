# Author : Jenn Schilling
# Title: #TidyTuesday Stranger Things
# Date: Oct 18 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(tidytext)

#### Get the Data ####

# Read in the data
stranger_things_all_dialogue <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

# Dialogue words
stranger_things_dialogue <- stranger_things_all_dialogue %>%
  unnest_tokens(word, dialogue) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words, by = "word") %>%
  count(season, episode, start_time, word) %>%
  mutate(start_time = lubridate::minute(start_time))

# Sentiment of dialogue
stranger_things_dialogue_s <- stranger_things_dialogue %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(season, episode, start_time, sentiment) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive - negative) 


#### Formatting ####

# Font
font_add_google(name = "Roboto Mono")
showtext_auto()
family <- "Roboto Mono"

# Theme Setup
font_color <- "#F2F2F2" 
title_color <- "#FF1515"
bcolor <- "#00090D"

#### Plot ####

ggplot(data = stranger_things_dialogue_s,
       mapping = aes(x = start_time,
                     y = sentiment,
                     fill = sentiment > 0)) +
  geom_hline(mapping = aes(yintercept = 0),
             color = font_color,
             size = 0.2) +
  geom_col() +
  facet_grid(season ~ episode,
             switch = "y") +
  scale_x_continuous(position = "top") +
  scale_fill_manual(values = c("#F22727", "#0F71F2"), guide = "none") +
  labs(x = "Episode",
       y = "Season",
       title = str_to_upper("Stranger Things"),
       subtitle = "Sentiment of the dialogue in each episode by minute.",
       caption  = "Data: 8flix.com prepped by Dan Fellowes & Jonathan Kitt | Design: Jenn Schilling") +
  # coord_cartesian(expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 30, angle = 0, vjust = 1, hjust = 0,
                                    color = font_color, family = family),
        axis.title.x = element_text(size = 30, hjust = 0, 
                                    color = font_color, family = family),
        
        strip.background = element_blank(),
        strip.text.y.left = element_text(size = 35, angle = 0, vjust = 1,
                                         color = font_color, family = family),
        strip.text.x = element_text(size = 35, color = font_color, 
                                    family = family),
        
        panel.grid = element_blank(),
        
        plot.background = element_rect(fill = bcolor, color = NA),
        panel.background = element_rect(fill = bcolor, color = NA),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 70, color = title_color,
                                  lineheight = 0.3, face = "bold", family = family),
        plot.subtitle = element_text(size = 20, color = font_color,
                                     lineheight = 0.3, face = "bold", family = family),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 20, color = font_color, 
                                    lineheight = 0.3, hjust = 1, family = family),
        
        plot.margin = margin(10, 10, 10, 10))
  


#### Save ####

ggsave(here("2022-10-18", "stranger_things.png"),
       plot = last_plot(),
       device = "png",
       width = 5,
       height = 5)



