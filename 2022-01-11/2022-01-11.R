# Author : Jenn Schilling
# Title: #TidyTuesday Bee Colonies
# Date: Jan 11 2022

#### Libraries ####

library(tidyverse)
library(ggforce)
library(extrafont)
library(ggtext)
library(scales)
library(patchwork)

#### Get the Data ####

colony <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

hexagon_dat_colony <- colony %>%
  filter(state == "United States") %>%
  group_by(state, months) %>%
  mutate(
  x0 = case_when(
    year == 2015 ~ 0,
    year == 2016 ~ 1.1*lag(colony_lost_pct, 1) + colony_lost_pct / 2,
    year == 2017 ~ 1.1*lag(colony_lost_pct, 2) + colony_lost_pct / 2,
    year == 2018 & months == "July-September" ~ lag(colony_lost_pct, 3) + 1.5*lag(colony_lost_pct, 1) + colony_lost_pct / 2, 
    year == 2018 ~ lag(colony_lost_pct, 3) + 2.05*lag(colony_lost_pct, 1) + colony_lost_pct / 2,
    year == 2019 & months == "January-March" ~ lag(colony_lost_pct, 4) + 1.9*lag(colony_lost_pct, 2) + colony_lost_pct / 2,
    year == 2019 ~ lag(colony_lost_pct, 4) + 1.8*lag(colony_lost_pct, 2) + colony_lost_pct / 2,
    year == 2020 ~ lag(colony_lost_pct, 5) + 1.7*lag(colony_lost_pct, 3) + colony_lost_pct / 2,
    year == 2021 ~ lag(colony_lost_pct, 6) + 2.05*lag(colony_lost_pct, 5) + lag(colony_lost_pct, 3) +  colony_lost_pct / 2
  ),
  y0 = case_when(
    year == 2015 ~ 0,
    year == 2016 ~ colony_lost_pct,
    year == 2017 ~ -colony_lost_pct,
    year == 2018 ~ lead(colony_lost_pct, 1) + colony_lost_pct,
    year == 2019 ~ 0,
    year == 2020 ~ -lag(colony_lost_pct, 1) - colony_lost_pct,
    year == 2021 ~ colony_lost_pct
  ),
  y = y0 - colony_lost_pct,
  months = factor(months, levels = c("January-March",
                                     "April-June",
                                     "July-September",
                                     "October-December")))

hexagon_dat_stressor <- stressor %>%
  filter(state == "United States") %>%
  group_by(state, months, stressor) %>%
  mutate(
    x0 = case_when(
      year == 2015 ~ 0,
      year == 2016 ~ 1.1*lag(stress_pct, 1) + stress_pct / 2,
      year == 2017 ~ 1.1*lag(stress_pct, 2) + stress_pct / 2,
      year == 2018 ~ lag(stress_pct, 3) + 2.05*lag(stress_pct, 1) + stress_pct / 2,
      year == 2019 ~ lag(stress_pct, 4) + 1.8*lag(stress_pct, 2) + stress_pct / 2,
      year == 2020 ~ lag(stress_pct, 5) + 1.7*lag(stress_pct, 3) + stress_pct / 2,
      year == 2021 ~ lag(stress_pct, 6) + 2.05*lag(stress_pct, 5) + lag(stress_pct, 3) +  stress_pct / 2
    ),
    y0 = case_when(
      year == 2015 ~ 0,
      year == 2016 ~ stress_pct,
      year == 2017 ~ -stress_pct,
      year == 2018 ~ lead(stress_pct, 1) + stress_pct,
      year == 2019 ~ 0,
      year == 2020 ~ -lag(stress_pct, 1) - stress_pct,
      year == 2021 ~ stress_pct
    ),
    y = y0 - stress_pct,
    months = factor(months, levels = c("January-March",
                                       "April-June",
                                       "July-September",
                                       "October-December")))

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

honey_pal <- c(
  "#E88600", # dark orange
  "#FBC220", # dark yellow
  "#FDD975" # light yellow
)


#### Plot ####

colony_plot <- ggplot(data = hexagon_dat_colony) +
  geom_regon(mapping = aes(x0 = x0,
                           y0 = y0,
                           sides = 6,
                           r = colony_lost_pct,
                           angle = 0,
                           fill = colony_n)) +
  scale_fill_continuous(low = honey_pal[2],
                        high = honey_pal[1],
                        labels = number_format(accuracy = 0.1, 
                                               scale = 1e-6,
                                               suffix = "M")) +
  geom_text(mapping = aes(x = x0,
                          y = y,
                          label = year),
            family = font,
            color = bcolor,
            size = 3,
            vjust = -1) +
  facet_wrap(~ months) +
  guides(fill = guide_colorbar(barwidth = unit(5, "cm"),
                               label.hjust = 0)) +
  labs(title = "Bee Colonies in the United States<br>",
       subtitle = "Size represents the percent of total colonies lost in the quarter.<br>Color represents the number of colonies.",
       fill = "") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_markdown(size = 16, color = fontcolor, family = title_font),)
  


stress_plot <- ggplot(data = hexagon_dat_stressor) +
  geom_line(mapping = aes(x = year,
                          y = stress_pct,
                          color = stressor),
            size = 1) +
  facet_wrap(~ months,
             scales = "free") +
  scale_color_manual(values = c(gray.colors(5, start = 0.3, end = 0.7), honey_pal[1])) +
  scale_x_continuous(limits = c(2016, 2021)) +
  scale_y_continuous(limits = c(0, 60),
                     labels = percent_format(accuracy = 1, 
                                             scale = 1)) +
  guides(color = "none") +
  labs(title = "",
       subtitle = "<span style = 'color:#E88600;'><b>Varroa mites</b></span> are the main stressor to bee colonies in the United States<br>Each line shows the percent of colonies affected by a stressor during the quarter.",
       x = "",
       y = "") +
  theme()
  

colony_plot + stress_plot +
  plot_annotation(caption = "Data: <b>USDA</b> | Design: <b>Jenn Schilling</b>")


ggsave("2022-01-11\\bee_colonies.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 14,
       height = 7,
       dpi = 500)
