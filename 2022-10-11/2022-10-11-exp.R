# Author : Jenn Schilling
# Title: #TidyTuesday Ravelry Yarn
# Date: Oct 11 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)


#### Get the Data ####

# Read in the data
yarn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')


# https://nimble-needles.com/wp-content/uploads/2021/09/how-to-knit-for-beginners.jpg

# Make stitch data



x_seq <- seq(0, 0.45, 0.15)
x_rep <- c(rep(x_seq[1], 2), rep(x_seq[2], 2), rep(x_seq[3], 2))
x <- rep(x_rep, 4)

xend_seq <- seq(1, 1.45, 0.15)
xend_rep <- c(rep(xend_seq[1], 2), rep(xend_seq[2], 2), rep(xend_seq[3], 2))
xend <- rep(xend_rep, 4)

y <- c(rep(c(0, 2), 3),
       rep(c(0, -2), 3),
       rep(c(2, 4), 3),
       rep(c(-2, -4), 3))

yend<- c(rep(1, 6),
       rep(-1, 6),
       rep(3, 6),
       rep(-3, 6))

stitches <- tibble(
  x = x,
  xend = xend,
  y = y,
  yend = yend
)


# stitches <- tibble(
#   x = c(0, 0, 0.15, 0.15, 0.3, 0.3,
#         0, 0, 0.15, 0.15, 0.3, 0.3,
#         0, 0, 0.15, 0.15, 0.3, 0.3,
#         0, 0, 0.15, 0.15, 0.3, 0.3),
#   xend = c(1, 1, 1.15, 1.15, 1.3, 1.3,
#            1, 1, 1.15, 1.15, 1.3, 1.3,
#            1, 1, 1.15, 1.15, 1.3, 1.3,
#            1, 1, 1.15, 1.15, 1.3, 1.3),
#   y = c(0, 2, 0, 2, 0, 2, 
#         0, -2, 0, -2, 0, -2, 
#         2, 4, 2, 4, 2, 4, 
#         -2, -4, -2, -4, -2, -4),
#   yend = c(1, 1, 1, 1, 1, 1, 
#            -1, -1, -1, -1, -1, -1, 
#            3, 3, 3, 3, 3, 3,
#            -3, -3, -3, -3, -3, -3)
# )

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

ggplot(data = stitches,
       mapping = aes(x = x,
                     xend = xend,
                     y = y,
                     yend = yend)) +
  geom_segment(size = 5,
               lineend = "round")

  theme(panel.background = element_rect(fill = bcolor, colour = NA),
        plot.background = element_rect(fill = bcolor, colour = NA),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 50, color = font_color, family = title_font, lineheight = 0.3, face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 20, color = font_color, lineheight = 0.3, hjust = 1, family = title_font),
       
        plot.margin = margin(10, 10, 10, 10))


#### Save ####

ggsave(here("2022-10-11", "yarn.png"),
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 7)



