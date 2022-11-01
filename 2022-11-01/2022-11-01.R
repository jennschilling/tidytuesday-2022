# Author : Jenn Schilling
# Title: #TidyTuesday 
# Date: Nov 1 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(lubridate)
library(showtext)

#### Get the Data ####

horror_movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

# Seperate out the genres into one row per genre per movie
horror_movies_genres <- horror_movies %>%
  separate_rows(genre_names, sep = ", ") %>%
  mutate(release_year = year(release_date))

# Count the number of movies of each genre per year
# Note: individual movies are counted more than once bc they can have multiple genres
horror_movies_genres_n <- horror_movies_genres %>%
  count(release_year, genre_names)

# Get the distinct number of genres for the color palette
num_genres <- horror_movies_genres %>%
  distinct(genre_names) %>%
  nrow()

# Get information about frequencies of genres for subtitle [added after live stream]
genre_counts <- horror_movies_genres %>%
  count(genre_names) %>%
  arrange(-n)

#### Formatting ####


# Get a color palette of reds, one for each genre
movie_genre_pal <- colorRampPalette(c("#800E05", "#FF6357"))(num_genres)

# Font
font_add_google(name = "Creepster")
font_add_google(name = "Roboto")
showtext_auto()

# Theme Setup
font <- "Roboto"
title_font <- "Creepster"
font_color <- "#F2F2F2" 
bcolor <- "#0D0D0D"

#### Plot ####

ggplot(data = horror_movies_genres_n,
       mapping = aes(x = release_year,
                     y = n,
                     fill = reorder(genre_names, -n))) +
  geom_col() +
  scale_y_reverse(labels = comma) +
  scale_fill_manual(values = movie_genre_pal,
                    guide = guide_legend(nrow = 2)) +
  labs( title = "The Many Genres of Horror Movies",
       # After live stream, added caption to add context for the plot
       subtitle = "Movies mostly have genres of horror or thriller. Comedy, drama, and mystery are common sub-genres.",
       x = "Year",
       y = "Number of Movies",
       # After live stream, added extra space above caption for the legend
       caption = "\n\nMovies are counted in multiple genres.\nData: The Movie Database | Design: Jenn Schilling") +
  theme(legend.position = c(0.45, -0.3), # After live stream, adjusted legend position to be further left and below plot
        # Argument below added after the live stream to remove legend title
        legend.title = element_blank(),
        legend.box.background = element_rect(fill = bcolor, color = NA),
        legend.background = element_rect(fill = bcolor, color = NA),
        legend.key = element_rect(fill = bcolor, color = NA),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        # Argument below added after live stream to bring text closer
        legend.spacing.x = unit(0.1, "cm"), 
        legend.text = element_text(family = font, color = font_color, 
                                   size = 12, hjust = 0),
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = bcolor, color = NA),
        plot.background = element_rect(fill = bcolor, color = NA),
        
        text = element_text(family = font, color = font_color),
        
        axis.ticks = element_line(color = font_color, size = 0.1),
        axis.title = element_text(family = font, color = font_color, size = 15),
        axis.text = element_text(family = font, color = font_color, size = 12),
        
        plot.title = element_text(family = title_font, color = font_color,
                                  size = 65, lineheight = 0.6, hjust = 0.5),
        plot.subtitle = element_text(family = font, color = font_color, size = 19),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, color = font_color, size = 14))

  

  


#### Save ####

ggsave(here("2022-11-01", "horror.png"),
       plot = last_plot(),
       device = "png",
       width = 4,
       height = 3)


