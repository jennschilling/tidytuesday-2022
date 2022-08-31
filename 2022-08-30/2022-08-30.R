# Author : Jenn Schilling
# Title: #TidyTuesday Pell Awards
# Date: Aug 30 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(geofacet)

#### Get the Data ####

pell <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')


#### Explore and Aggregate the Data ###

pell_agg <- pell %>%
  group_by(STATE, YEAR) %>%
  summarize(TOTAL_AWARD = sum(AWARD),
            TOTAL_RECIPIENT = sum(RECIPIENT),
            TOTAL_INST = n()) %>%
  ungroup()

states_over_100k <- pell_agg %>%
  mutate(rec_check = TOTAL_RECIPIENT > 100000) %>%
  count(STATE, rec_check) %>%
  filter(n > 13 & rec_check == TRUE) %>%
  select(STATE, rec_check)

pell_agg <- left_join(pell_agg, states_over_100k, by = "STATE")


#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#e9eff4"


#### Plot ####

ggplot(data = pell_agg,
       mapping = aes(x = YEAR,
                     y = TOTAL_RECIPIENT,
                     group = STATE,
                     color = rec_check)) +
  geom_line(size = 0.6) +
  facet_geo(~STATE) +
  scale_y_continuous(labels = comma_format(scale = 1e-3, big.mark = ",", suffix = "K")) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015),
                     labels = c("'00", "'05", "'10", "'15")) +
  scale_color_manual(values = c("#f18805", line_color)) +
  guides(color = "none") +
  labs(title = "The number of Pell Grant recipients increased consistently until 2011.",
       subtitle = "\nU.S. Federal Pell Grants support first-time undergraduate students who have exceptional financial need.  In 2009, there was a large increase in the number\nof recipients mainly due to the Great Recession and a change in federal law. The Great Recession caused more students to enroll in college and more\nstudents to meet income eligibility requirements. A change in federal law also increased to the income threshold for full Pell Grant qualification.\nHowever, in 2011, federal law changed again and the income threshold was lowered, meaning fewer students were eligible.\n\nNumber of Pell Grant recipients from 1999 through 2017. States in orange enrolled more than 100,000 Pell Recipients most years.\n",
       caption = "\nData U.S. Department of Education | Design Jenn Schilling") +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 12, color = font_color),
        axis.ticks = element_line(color = line_color, size = 0.1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = bcolor),
        strip.background = element_rect(fill = "#7EA4C2"),
        strip.text = element_text(size = 20, color = "white"),
        plot.title.position = "plot",
        plot.title = element_text(size = 50, color = font_color, lineheight = 0.3, face = "bold"),
        plot.subtitle = element_text(size = 26, color = font_color, lineheight = 0.3),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 14, color = font_color, hjust = 1),
        plot.margin = margin(10, 10, 10, 10))


#### Save ####

ggsave(here("2022-08-30", "pell.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 8.2,
       height = 6)  
