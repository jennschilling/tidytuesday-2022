# Author : Jenn Schilling
# Title: #TidyTuesday Open-Source Psychometrics Project 
# Date: Aug 16 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggradar)
library(ggimage)

#### Get the Data ####

characters <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psych_stats <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')
myers_briggs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/myers_briggs.csv')


myers_briggs <- myers_briggs %>%
  # Source: https://www.truity.com/page/16-personality-types-myers-briggs
  mutate(label = case_when(
    myers_briggs == "INTJ" ~ "Mastermind",
    myers_briggs == "INTP" ~ "Architect",
    myers_briggs == "ENTJ" ~ "Commander",
    myers_briggs == "ENTP" ~ "Visionary",
    myers_briggs == "INFJ" ~ "Counselor",
    myers_briggs == "INFP" ~ "Healer",
    myers_briggs == "ENFJ" ~ "Teacher",
    myers_briggs == "ENFP" ~ "Champion",
    myers_briggs == "ISTJ" ~ "Inspector",
    myers_briggs == "ISFJ" ~ "Protector",
    myers_briggs == "ESTJ" ~ "Supervisor",
    myers_briggs == "ESFJ" ~ "Provider",
    myers_briggs == "ISTP" ~ "Crafter",
    myers_briggs == "ISFP" ~ "Composer",
    myers_briggs == "ESTP" ~ "Dynamo",
    myers_briggs == "ESFP" ~ "Performer",
    TRUE ~ "None"))

sc_mb <- myers_briggs %>%
  filter(uni_name == "Schitt's Creek") %>%
  group_by(char_name) %>%
  mutate(max_match = max(avg_match_perc)) %>%
  ungroup() %>%
  filter(max_match == avg_match_perc) %>%
  group_by(char_name) %>%
  mutate(label_comb = ifelse(is.na(lag(label)), label, paste(label, lag(label), sep = "/")),
         mb_comb = ifelse(is.na(lag(myers_briggs)), myers_briggs, paste(myers_briggs, lag(myers_briggs), sep = "/")),
         label = paste(label_comb, mb_comb, sep = ", ")) %>%
  ungroup() %>%
  filter(!(char_name == "David Rose" & label_comb == "Visionary")) %>%
  left_join(characters , by = c("char_id" = "id", "char_name" = "name",
                                "uni_id", "uni_name"))

# Get personality characteristics of interest
sc_chars <- characters %>%
  filter(uni_name == "Schitt's Creek") %>%
  left_join(psych_stats, by = c("id" = "char_id", "name" = "char_name",
                                "uni_id" = "uni_id", "uni_name" = "uni_name")) %>%
  filter(question %in% c("dramatic/no-nonsense", "emotional/logical", "confident/insecure",
                         "cautious/impulsive", "idealist/realist", "forgiving/vengeful")) %>%
  mutate(avg_rating_adj = ifelse(personality %in% c("impulsive", "insecure", "no-nonsense", 
                                                    "logical", "vengeful", "realist"),
                                 100 - avg_rating, avg_rating),
         personality_adj = case_when(
           personality == "impulsive" ~ "cautious",
           personality == "insecure" ~ "confident",
           personality == "no-nonsense" ~ "dramatic",
           personality == "logical" ~ "emotional",
           personality == "realist" ~ "idealist",
           personality == "vengeful" ~ "forgiving",
           TRUE ~ personality))

sc_chars_radar <- sc_chars %>%
  select(name, image_link, avg_rating_adj, personality_adj) %>%
  mutate(name = factor(name, levels = c("Alexis Rose", "Ted Mullens",
                                        "Moira Rose", "Johnny Rose",
                                        "David Rose", "Stevie Budd")),
         avg_rating_adj = avg_rating_adj / 100,
         group = name,
         personality_adj = str_to_sentence(personality_adj)) %>%
  pivot_wider(names_from = personality_adj,
              values_from = avg_rating_adj)

#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
font_color <- "#FFFFFF" # white
line_color <- "#FFFFFF" # white
bcolor <- "#000000" # black


#### Plot 1 ####

p <- ggradar(plot.data = sc_chars_radar %>% select(-image_link, -name),
        font.radar = font,
        background.circle.transparency = 0,
        gridline.min.colour = line_color,
        gridline.mid.colour = line_color,
        gridline.max.colour = line_color,
        axis.line.colour = line_color) +
  facet_wrap(vars(group), nrow = 3) +
  scale_color_manual(values = rep("#F2BB16", 6)) +
  guides(color = "none") +
  labs(title = str_to_upper("\nOpposites Attract in Schitt's Creek\n"),
       caption = "\nData Open-Source Psychometrics Project | Design Jenn Schilling\n") +
  theme(plot.background = element_rect(fill = bcolor, color = NA),
        panel.background = element_rect(fill = bcolor, color = NA),
        text = element_text(size = 30, color = font_color),
        strip.text = element_text(size = 80),
        plot.title = element_text(size = 150, color = font_color, lineheight = 0.2),
        plot.caption = element_text(size = 50, color = font_color),
        plot.margin = margin(10, 10, 10, 10))


# Change the text to white for labels and remove text for percents

## CODE FROM: https://stackoverflow.com/questions/72205539/change-axis-title-text-color-in-ggradar

# Text layers - labels
is_text <- sapply(p$layers, function(x) inherits(x$geom, "GeomText") && any(x$data$text %in% names(sc_chars_radar)))

# Change to white
p$layers[is_text] <- lapply(p$layers[is_text], function(x) { x$aes_params$colour <- "white"; x})
# Make larger
p$layers[is_text] <- lapply(p$layers[is_text], function(x) { x$aes_params$size <- 20; x})

# Text layers - percentages
is_text <- sapply(p$layers, function(x) inherits(x$geom, "GeomText") && any(x$data$text %in% c(0, 0.5, 1)))

# Change to NA
p$layers[is_text] <- lapply(p$layers[is_text], function(x) { x$aes_params$colour <- NA; x})

p

#### Save ####

ggsave(here("2022-08-16", "schitts.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 15,
       height = 22)  

#### Plot 2 ####

ggplot(data = sc_mb,
       mapping = aes(x = "0",
                     y = "0",
                     label = label,
                     image = image_link)) +
  geom_image(size = 0.9) +
  geom_text(family = font,
            color = "#F2BB16",
            size = 10,
            nudge_y = 0.6,
            nudge_x = -0.57,
            hjust = 0,
            lineheight = 0.3) +
  facet_wrap(~ char_name) +
  labs(title = "Schitt's Creek Personality Chart\n",
       caption = "Based on Myers-Briggs personality types.\nData Open-Source Psychometrics Project | Design Jenn Schilling") +
  coord_cartesian(clip = "off") +
  theme(plot.background = element_rect(fill = bcolor, color = NA),
        panel.background = element_rect(fill = bcolor, color = NA),
        text = element_text(size = 20, color = font_color),
        strip.text = element_text(size = 40, hjust = 0, color = font_color),
        strip.background = element_blank(),
        plot.title = element_text(size = 70, color = "#F2BB16", lineheight = 0.05),
        plot.subtitle = element_text(size = 30, color = font_color, lineheight = 0.2),
        plot.caption = element_text(size = 20, color = font_color, lineheight = 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(10, 10, 10, 10))

#### Save ####

ggsave(here("2022-08-16", "schitts_2.png"),
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 7.2,
       height = 6)  

