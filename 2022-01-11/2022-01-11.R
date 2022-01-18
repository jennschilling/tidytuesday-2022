# Author : Jenn Schilling
# Title: #TidyTuesday Bee Colonies
# Date: Jan 11 2022

#### Libraries ####

library(tidyverse)
library(ggforce)
library(geofacet)

#### Get the Data ####

colony <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

hexagon_dat <- colony %>%
  filter(state == "United States" & months == "January-March") %>%
  mutate(
  x0 = case_when(
    year == 2015 ~ 0,
    year == 2016 ~ lag(colony_lost_pct, 1) + colony_lost_pct / 2,
    year == 2017 ~ lag(colony_lost_pct, 2) + colony_lost_pct / 2,
    year == 2018 ~ 2*lag(colony_lost_pct, 3) + lag(colony_lost_pct, 1) + colony_lost_pct / 2,
    year == 2019 ~ 2*lag(colony_lost_pct, 4) + lag(colony_lost_pct, 2) + colony_lost_pct / 2,
    year == 2020 ~ 2*lag(colony_lost_pct, 5) + lag(colony_lost_pct, 3) + colony_lost_pct / 2,
    year == 2021 ~ 2*lag(colony_lost_pct, 6) + 2*lag(colony_lost_pct, 5) + lag(colony_lost_pct, 2) +  colony_lost_pct / 2
  ),
  y0 = case_when(
    year == 2015 ~ 0,
    year == 2016 ~ colony_lost_pct,
    year == 2017 ~ -colony_lost_pct,
    year == 2018 ~ lead(colony_lost_pct, 1) + colony_lost_pct,
    year == 2019 ~ 0,
    year == 2020 ~ -lag(colony_lost_pct, 1) - colony_lost_pct,
    year == 2021 ~ 0
  ))


#### Plot ####

ggplot(data = hexagon_dat %>% filter(months == "January-March")) +
  geom_regon(mapping = aes(x0 = x0,
                           y0 = y0,
                           sides = 6,
                           r = colony_lost_pct,
                           angle = 0,
                           fill = year),
             alpha = 0.5) +
  geom_text(mapping = aes(x = x0,
                          y = y0,
                          label = year))
  
  # geom_col(mapping = aes(x = stressor,
  #                        y = stress_pct,
  #                        fill = stressor)) +
  
