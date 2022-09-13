# Author : Jenn Schilling
# Title: #TidyTuesday Bigfoot
# Date: Sep 13 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(tidytext)
library(ggwordcloud)
library(png)

#### Get the Data ####

bigfoot <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bfro_reports_geocoded.csv')


#### Get the Image ####
bigfoot_img <- readPNG(here("2022-09-13", "bigfoot.png"), native =  TRUE)

#### Text Analysis ####

# Get the words in the observations
bigfoot_obs <- bigfoot %>%
  select(observed, number) %>%
  unnest_tokens(word, observed) %>%
  anti_join(stop_words)

# Count the word frequency
bigfoot_obs_freq <- bigfoot_obs %>%
  count(word) %>%
  arrange(-n)

# Count the number of observations each word was in
bigfoot_obs_f <- bigfoot_obs %>%
  distinct(number, word) %>%
  count(word) %>%
  arrange(-n) %>%
  mutate(is_number = parse_number(word)) %>%
  filter(is.na(is_number))


# Get the bi-gram in the observations
bigfoot_obs_2 <- bigfoot %>%
  select(observed, number) %>%
  unnest_tokens(word, observed, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"))

# Remove stop words
bigfoot_obs_2 <- bigfoot_obs_2 %>%
  anti_join(stop_words, by = c("word1" = "word")) %>%
  anti_join(stop_words, by = c("word2" = "word")) %>%
  unite("word", word1:word2, sep = " ")

# Count the number of observations each word was in
bigfoot_obs_f_2 <- bigfoot_obs_2 %>%
  distinct(number, word) %>%
  count(word) %>%
  arrange(-n) 

# What do people hear?
bigfoot_obs_f_2_heard <- bigfoot_obs_f_2 %>%
  mutate(contains_heard = str_detect(word, "heard|hear")) %>%
  filter(contains_heard) %>%
  separate(word, c("word1", "word2")) %>%
  filter(grepl("hear", word1)) %>%
  mutate(is_number = parse_number(word2)) %>%
  filter(is.na(is_number)) %>%
  group_by(word2) %>%
  summarize(n = sum(n))


# 38 of the 5,021 rows are missing observed text
bigfoot_obs_missing_check <- bigfoot %>%
  filter(is.na(observed))

#### Formatting ####

# Font
font_add_google(name = "Montserrat")
font_add_google(name = "Carter One")
showtext_auto()

# Theme Setup
font <- "Montserrat"
title_font <- "Carter One"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#e9eff4"


# Color Palette
bf_green <- c("#007502", "#003601")



#### Plot ####

wc <- ggplot(data = bigfoot_obs_f_2_heard %>% filter(n > 1),
       mapping = aes(label = word2, 
                     size = n,
                     color = n)) +
  geom_text_wordcloud(family = title_font,
                      shape = "square",
                      rm_outside = TRUE) +
  scale_size_area(max_size = 14) +
  scale_color_gradient(low = bf_green[1],
                       high = bf_green[2]) +
  theme(panel.background = element_rect(fill = bcolor, colour = NA),
        plot.background = element_rect(fill = bcolor, colour = NA))

bf <- ggplot() +
  inset_element(bigfoot_img, 
                0, 0, 1.01, 1.01,
                align_to = "full") +
  coord_cartesian(expand = FALSE) +
  theme(panel.background = element_rect(fill = bcolor, colour = bcolor),
        plot.background = element_rect(fill = bcolor, colour = bcolor))

bf + wc +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(title = str_to_upper("What do people hear when they see bigfoot?"),
                  caption = "\nData: Bigfoot Field Researchers Organization (BFRO) by way of Data.World | Design: Jenn Schilling",
                  theme = theme(panel.background = element_rect(fill = bcolor, colour = NA),
                                plot.background = element_rect(fill = bcolor, colour = NA),
                                plot.title.position = "plot",
                                plot.title = element_text(size = 38, color = font_color, family = title_font, face = "bold"),
                                plot.caption.position = "plot",
                                plot.caption = element_text(size = 14, color = font_color, lineheight = 0.3, hjust = 1, family = title_font),
                                plot.margin = margin(10, 10, 10, 10))) 

#### Save ####

ggsave(here("2022-09-13", "bigfoot_wc.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 4.5,
       height = 3.5)  



# Could plot in the image of bigfoot, but I wasn't very successful
# https://lepennec.github.io/ggwordcloud/