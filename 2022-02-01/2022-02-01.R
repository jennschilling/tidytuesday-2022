# Author : Jenn Schilling
# Title: #TidyTuesday Dog Breeds
# Date: Feb 1 2022

#### Libraries ####

library(tidyverse)
library(ggforce)
library(extrafont)
library(ggtext)
library(ggbump)
library(janitor)
library(ggimage)

#### Get the Data ####

breed_traits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# Get top 10 dogs
top_10 <- breed_rank_all %>%
  clean_names() %>%
  pivot_longer(x2013_rank:x2020_rank,
               names_to = "year",
               values_to = "rank") %>%
  mutate(year = parse_number(year),
         # This is really just to put Dachsunds below everything else since they only appear in 2013 and 2020 and I don't want the between those years to show
         breed = factor(breed,
                        levels = c("Dachshunds",
                                   "Retrievers (Labrador)",
                                   "German Shepherd Dogs",
                                   "Retrievers (Golden)",
                                   "Beagles",
                                   "Bulldogs",
                                   "Yorkshire Terriers",
                                   "Boxers",
                                   "Poodles",
                                   "Rottweilers",
                                   "Pointers (German Shorthaired)",
                                   "Pembroke Welsh Corgis",
                                   "French Bulldogs"))) %>%
  filter(rank <= 10) %>%
  group_by(breed) %>%
  mutate(first_year = year == min(year),
         last_year = year == max(year)) %>%
  ungroup() 

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
fontcolor <- "gray30"
bcolor <- "white"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title.y = element_text(size = 12, color = fontcolor, angle = 0, vjust = 0.98),
  axis.title.x = element_blank(),
  axis.text = element_text(size = 12, color = fontcolor),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 20, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 12, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

ggplot() +
  geom_bump(data = top_10,
            mapping = aes(x = year,
                          y = rank,
                          group = breed,
                          color = breed),
            smooth = 15, size = 2.5) +
  geom_image(data = top_10 %>% filter(first_year == TRUE | last_year == TRUE),
             mapping = aes(x = year,
                           y = rank,
                           image = image),
             size = 0.07) +
  geom_text(data = top_10 %>% filter(first_year == TRUE | last_year == TRUE),
            mapping = aes(x = year,
                          y = rank,
                          label = breed),
            nudge_y = -0.5,
            color = fontcolor,
            family = font,
            size = 3) +
  scale_y_reverse(breaks = seq(from = 1, to = 10)) +
  scale_x_continuous(breaks = seq(from = 2013, to = 2020),
                     limits = c(2012.7, 2021.2)) +
  # scale_color_manual(values = c("#FFFFFF", "#208eb7", "#7b3f5b", "#7ec993", "#1c5f1e", 
  #                               "#bcaff9", "#5826a6", "#9bc732", "#5064be", 
  #                               "#cd49dc", "#af2168", "#f372a8", "#1c4c5e")) +
  
  scale_color_manual(values = c("#FFFFFF", "#208eb7", "#C0C0C0", "#B0B0B0", 
                                "#A8A8A8", "#DCDCDC", "#888888", "#787878", 
                                "#D0D0D0", "#989898", "#707070", "#7ec993")) +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(y = "Rank",
       title = "Labrador Retreievers are consistently the most popular dog while French Bulldogs have made large gains in popularity.<br>",
       caption = "<br>Data: <b>American Kennel Club</b> | Design: <b>Jenn Schilling</b>")

ggsave("2022-02-01\\dog_breeds.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 14,
       height = 10,
       dpi = 500)  
