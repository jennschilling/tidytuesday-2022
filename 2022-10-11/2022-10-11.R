# Author : Jenn Schilling
# Title: #TidyTuesday Ravelry Yarn
# Date: Oct 11 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggridges)
library(patchwork)
library(png)

#### Get the Data ####

# Read in the data
yarn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# Processing
yarn_p <- yarn %>%
  mutate(yarn_weight_name = ifelse(is.na(yarn_weight_name), "None", yarn_weight_name),
         yarn_weight_name = factor(yarn_weight_name,
                                   levels = c("None", 
                                              "No weight specified",
                                              "Jumbo",
                                              "Super Bulky",
                                              "Bulky",
                                              "Aran", 
                                              "Aran / Worsted",
                                              "Worsted",
                                              "DK",
                                              "DK / Sport",
                                              "Sport",
                                              "Fingering",
                                              "Light Fingering",
                                              "Lace",
                                              "Cobweb",
                                              "Thread"))) %>%
  filter(!is.na(rating_average)) %>%
  group_by(yarn_weight_name) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(yarn_weight_name_label = ifelse(yarn_weight_name == "Thread",
                                         paste0(yarn_weight_name, " (Number of patterns = ", comma(n), ')'),
                                         paste0(yarn_weight_name, " (N = ", comma(n), ')')))

avg_rating <- mean(yarn_p$rating_total / yarn_p$rating_count)


#Wool icons created by Freepik - Flaticon

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

ggplot() +
  geom_vline(mapping = aes(xintercept = avg_rating),
             color = line_color,
             size = 0.2) +
  geom_density_ridges(data = yarn_p %>% filter(yarn_weight_name != "None" & 
                                                 yarn_weight_name != "No weight specified"),
                      mapping = aes(x = rating_average,
                                    y = yarn_weight_name,
                                    fill = yarn_weight_name),
                      alpha = 0.6,
                      color = NA) +
  annotate("text",
           x = avg_rating,
           y = "Jumbo",
           label = "Overall average rating",
           hjust = 1.05,
           vjust = 2,
           family = font,
           color = font_color,
           size = 4) +
  geom_text(data = yarn_p %>% filter(yarn_weight_name != "None" & 
                                       yarn_weight_name != "No weight specified") %>%
              distinct(yarn_weight_name, yarn_weight_name_label),
            mapping = aes(x = 0.6,
                          y = yarn_weight_name,
                          label = yarn_weight_name_label,
                          color = yarn_weight_name),
            vjust = 0,
            hjust = 0,
            nudge_y = 0.1,
            nudge_x = -0.2,
            family = font,
            size = 5) +
  guides(fill = "none", 
         color = "none") +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  scale_color_manual(values = c("#123622", "#002a00", "#0a3055", "#000036",
                                "#360036", "#322a60", "#34515e", "#4f5a65",
                                "#8d6708", "#802200", "#d35400", "#e63022",
                                "#870c25", "#dc143c")) +
  scale_fill_manual(values = c("#123622", "#002a00", "#0a3055", "#000036",
                               "#360036", "#322a60", "#34515e", "#4f5a65",
                               "#8d6708", "#802200", "#d35400", "#e63022",
                               "#870c25", "#dc143c")) +
  coord_cartesian(clip = "off") +
  labs(x = "Average Rating of Pattern",
       title = "Ravelry patterns in lighter weight yarns\ntend to have higher average ratings.",
       caption = "Data: ravelry.com by way of Alice Walsh | Design: Jenn Schilling") +
  theme(panel.background = element_rect(fill = bcolor, colour = NA),
        plot.background = element_rect(fill = bcolor, colour = NA),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 30, color = font_color, family = title_font, 
                                  lineheight = 0.3, face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 12, color = font_color, lineheight = 0.3, 
                                    hjust = 1, family = title_font),
       
        plot.margin = margin(10, 2, 10, 2),
        
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        axis.text.x = element_text(size = 12, family = font, color = font_color, vjust = 1),
        axis.title.x = element_text(size = 14, family = font, color = font_color),
        axis.ticks.x = element_line(size = 0.2, color = "white"),
        
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))


#### Save ####

ggsave(here("2022-10-11", "yarn_plot.png"),
       plot = last_plot(),
       device = "png",
       width = 3,
       height = 3)



