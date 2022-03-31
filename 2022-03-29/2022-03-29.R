# Author : Jenn Schilling
# Title: #TidyTuesday Collegiate sports
# Date: Mar 28 2022

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Get the Data ####

sports <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

#### Process the Data ####

# Get the participation, revenue, and expenditures in each sport each year
sports_sub <- sports %>%
  group_by(year) %>%
  summarise(total_part_men = sum(sum_partic_men, na.rm = TRUE),
            total_part_women = sum(sum_partic_women, na.rm = TRUE),
            med_rev_men = median(rev_men, na.rm = TRUE),
            med_rev_women = median(rev_women, na.rm = TRUE),
            med_exp_men = median(exp_men, na.rm = TRUE),
            med_exp_women = median(exp_women, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(total_part_men:med_exp_women) %>%
  separate(name, c("metric", "measure", "gender"), "_") %>%
  mutate(metric = paste(metric, measure, sep = "_")) %>%
  select(-measure)

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
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

ggplot() +
  geom_col(data = sports_sub %>% filter(metric == "total_part" & gender == "men"),
           mapping = aes(y = value,
                         x = year,
                         fill = gender),
           width = 1,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "total_part" & gender == "women"),
           mapping = aes(y = value,
                         x = year,
                         fill = gender),
           width = 0.75,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "total_part" & gender == "men"),
           mapping = aes(y = -value,
                         x = year,
                         fill = gender),
           width = 1,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "total_part" & gender == "women"),
           mapping = aes(y = -value,
                         x = year,
                         fill = gender),
           width = 0.75,
           position = position_jitter(width = 0.1, height = 0)) +
  
  
  geom_col(data = sports_sub %>% filter(metric == "med_exp" & gender == "men"),
           mapping = aes(y = value,
                         x = year,
                         fill = gender),
           width = 1,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "med_exp" & gender == "women"),
           mapping = aes(y = value,
                         x = year,
                         fill = gender),
           width = 0.75,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "med_exp" & gender == "men"),
           mapping = aes(y = -value,
                         x = year,
                         fill = gender),
           width = 1,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "med_exp" & gender == "women"),
           mapping = aes(y = -value,
                         x = year,
                         fill = gender),
           width = 0.75,
           position = position_jitter(width = 0.1, height = 0)) +
  
  
  geom_col(data = sports_sub %>% filter(metric == "med_rev" & gender == "men"),
           mapping = aes(y = value,
                         x = year,
                         fill = gender),
           width = 1,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "med_rev" & gender == "women"),
           mapping = aes(y = value,
                         x = year,
                         fill = gender),
           width = 0.75,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "med_rev" & gender == "men"),
           mapping = aes(y = -value,
                         x = year,
                         fill = gender),
           width = 1,
           position = position_jitter(width = 0.1, height = 0)) +
  geom_col(data = sports_sub %>% filter(metric == "med_rev" & gender == "women"),
           mapping = aes(y = -value,
                         x = year,
                         fill = gender),
           width = 0.75,
           position = position_jitter(width = 0.1, height = 0)) +
  
  facet_wrap(~metric, ncol = 1) +
  
  scale_fill_manual(values = c("#02735e", "#fd7d00")) +
  
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  theme_void() +
  theme(strip.text = element_blank(),
        panel.spacing = unit(0, "lines"))

