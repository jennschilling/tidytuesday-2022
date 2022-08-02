# Author : Jenn Schilling
# Title: #TidyTuesday Oregon Spotted Frog
# Date: Aug 2 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(patchwork)

#### Get the Data ####

frog <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

# Set labels and levels for the sites
frog <- frog %>%
  mutate(Subsite_n = str_replace(Subsite, "Res", "Reservoir"),
         Subsite_fct = factor(Subsite_n,
                          levels = c("W Reservoir", "N Reservoir",
                                     "Cow Camp Pond", "Cow Camp River",
                                     "SE Pond", "NE Reservoir"))) 

# Create labels for the sites
labs <- tibble(
  label = c("Cow Camp Pond\nand River", 
            "N Reservoir",
            "NE Reservoir", 
            "SE Pond", 
            "W Reservoir"),
  x = c(598400, 
        597500, 
        599100, 
        597400, 
        595400),
  y = c(4851000, 
        4851400, 
        4852000, 
        4846700, 
        4850500)
)

#### Formatting ####

# Frog colors
frog_colors <- colorRampPalette(c("#6D8C4D", "#C4D977",  "#A6654E", "#593528"))(32)

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#e9eff4"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = line_color),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 26, color = font_color),
  axis.text = element_text(size = 26, color = font_color),
  axis.ticks = element_line(color = line_color),
  
  strip.text = element_text(size = 26, color = font_color, hjust = 0),
  
  legend.text = element_text(size = 26, color = font_color),
  legend.title = element_text(size = 26, color = font_color),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 32, color = font_color, lineheight = 0.3),
  
  plot.subtitle = element_text(size = 26, color = font_color, lineheight = 0.3),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 16, color = font_color, hjust = 1)
)


#### Explore Data ####

# How many individual frogs are in the data?
frog_id <- frog %>%
  count(Frequency)
 

# How many frogs are in each subsite
frog_s <- frog %>%
  select(Subsite, Frequency) %>%
  unique() %>%
  count(Subsite)

#### Plot ####

# Whole Area
 area <- ggplot() +
  geom_point(data = frog %>% filter(Interval == 0),
             mapping = aes(x = UTME_83,
                           y = UTMN_83,
                           group = Frequency,
                           color = as.character(Frequency)),
             size = 2) +
  geom_point(data = frog %>% 
               group_by(Frequency) %>% 
               mutate(max_int = max(Interval)) %>%
               filter(Interval == max_int),
             mapping = aes(x = UTME_83,
                           y = UTMN_83,
                           group = Frequency,
                           color = as.character(Frequency)),
             size = 2) +
  geom_path(data = frog,
            mapping = aes(x = UTME_83,
                          y = UTMN_83,
                          group = Frequency,
                          color = as.character(Frequency))) +
  geom_text(data = labs,
            mapping = aes(x = x,
                          y = y,
                          label = label),
            family = font,
            color = font_color,
            size = 10,
            hjust = 0.5,
            vjust = 0.5,
            lineheight = 0.4) +
  scale_color_manual(values = frog_colors) +
  guides(color = "none") +
  coord_cartesian(clip = "off") +
  labs(x = "",
       y = "",
       title = "Crane Prairie Reservoir in Oregon, USA") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank())



# Small Multiples
sites <- ggplot() +
  geom_point(data = frog %>% filter(Interval == 0),
             mapping = aes(x = UTME_83,
                           y = UTMN_83,
                           group = Frequency,
                           color = as.character(Frequency)),
             size = 2) +
  geom_point(data = frog %>% 
               group_by(Frequency) %>% 
               mutate(max_int = max(Interval)) %>%
               filter(Interval == max_int),
             mapping = aes(x = UTME_83,
                           y = UTMN_83,
                           group = Frequency,
                           color = as.character(Frequency)),
             size = 2) +
  geom_path(data = frog,
            mapping = aes(x = UTME_83,
                          y = UTMN_83,
                          group = Frequency,
                          color = as.character(Frequency))) +
  facet_wrap(~ Subsite_fct,
             scales = "free",
             nrow = 1) +
  scale_color_manual(values = frog_colors) +
  guides(color = "none",
         shape = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank())

# Put plots together (one on top of the other)
area / sites +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(title = str_to_upper("Tracking Oregon Spotted Frogs"),
                  subtitle = "\nThe movements of 32 different frogs in the Crane Prairie Reservoir were tracked in Fall 2018.\n",
                  caption = "Data: USGS | Design: Jenn Schilling",
                  theme = theme(plot.title = element_text(size = 80, color = font_color, lineheight = 0.3),
                                plot.subtitle = element_text(size = 34, color = font_color, lineheight = 0.3),
                                plot.margin = margin(20, 20, 20, 20)))


# Save

ggsave(here("2022-08-02", "frogs.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 7.5,
       height = 7.5)  

