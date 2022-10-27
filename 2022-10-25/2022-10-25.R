# Author : Jenn Schilling
# Title: #TidyTuesday Great British Bake Off
# Date: Oct 25 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggbump)
library(patchwork)
library(bakeoff)

#### Get the Data ####

View(challenges)
View(bakers)
View(episodes)

median_age <- median(bakers$age)

winners <- bakers %>%
  filter(series_winner == 1) %>%
  left_join(episodes, by = "series") %>%
  mutate(star_baker = str_detect(baker, sb_name),
         star_baker = ifelse(is.na(star_baker), FALSE, star_baker),
         baker_label = paste(baker, "| Series:", series),
         med_age_compare = age > median_age,
         age_bin = case_when(
           age < 20 ~ "<20",
           age < 30 ~ "20-29", # min age of winners is 22
           age < 40 ~ "30-39",
           age < 50 ~ "40-49",
           TRUE ~ "50+")) %>% # max age of winners is 60
  select(series, episode, bakers_appeared, baker, baker_label, 
         baker_full, age, med_age_compare, age_bin,
         occupation, hometown, star_baker) %>%
  left_join(challenges, by = c("baker", "series", "episode"))


# Set up lines for age bins
winners_age <- winners %>%
  distinct(baker_label, series, age, age_bin, med_age_compare) 

lines <- tibble(
  
  baker_label = rep(winners_age$baker_label, 4),
  age = rep(winners_age$age, 4),
  age_bin = rep(winners_age$age_bin, 4),
  med_age_compare = rep(winners_age$med_age_compare, 4),
  series = rep(winners_age$series, 4),
  
  x = c(rep(5, 10), rep(5.5, 10),
        rep(10, 10), rep(10, 10)),
  
  y = c(rep(12, 10), rep(12, 10),
        rep(6.5, 10), rep(6, 10)),
  
  line = rep(1, 40)) %>%
  
  bind_rows(
    
    tibble(
      
      baker_label = rep(winners_age %>% filter(age_bin != "20-29") %>% pull(baker_label), 4),
      age = rep(winners_age %>% filter(age_bin != "20-29") %>% pull(age), 4),
      age_bin = rep(winners_age %>% filter(age_bin != "20-29") %>% pull(age_bin), 4),
      med_age_compare = rep(winners_age %>% filter(age_bin != "20-29") %>% pull(med_age_compare), 4),
      series = rep(winners_age %>% filter(age_bin != "20-29") %>% pull(series), 4),
      
      x = c(rep(6, nrow(winners_age %>% filter(age_bin != "20-29"))), 
            rep(6.5, nrow(winners_age %>% filter(age_bin != "20-29"))),
            rep(10, nrow(winners_age %>% filter(age_bin != "20-29"))), 
            rep(10, nrow(winners_age %>% filter(age_bin != "20-29")))),
      
      y = c(rep(12, nrow(winners_age %>% filter(age_bin != "20-29"))), 
            rep(12, nrow(winners_age %>% filter(age_bin != "20-29"))),
            rep(7.5, nrow(winners_age %>% filter(age_bin != "20-29"))), 
            rep(7, nrow(winners_age %>% filter(age_bin != "20-29")))),
      
      line = rep(2, nrow(winners_age %>% filter(age_bin != "20-29")) * 4))) %>%
  
  bind_rows(
    
    tibble(
      
      baker_label = rep(winners_age %>% filter(age_bin %in% c("40-49", "50+")) %>% pull(baker_label), 4),
      age = rep(winners_age %>% filter(age_bin %in% c("40-49", "50+")) %>% pull(age), 4),
      age_bin = rep(winners_age %>% filter(age_bin %in% c("40-49", "50+")) %>% pull(age_bin), 4),
      med_age_compare = rep(winners_age %>% filter(age_bin %in% c("40-49", "50+")) %>% pull(med_age_compare), 4),
      series = rep(winners_age %>% filter(age_bin %in% c("40-49", "50+")) %>% pull(series), 4),
      
      x = c(rep(7, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+")))), 
            rep(7.5, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+")))),
            rep(10, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+")))), 
            rep(10, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+"))))),
      
      y = c(rep(12, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+")))), 
            rep(12, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+")))),
            rep(8.5, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+")))), 
            rep(8, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+"))))),
      
      line = rep(3, nrow(winners_age %>% filter(age_bin %in% c("40-49", "50+"))) * 4))) %>%
  
  bind_rows(
    
    tibble(
      
      baker_label = rep(winners_age %>% filter(age_bin == "50+") %>% pull(baker_label), 4),
      age = rep(winners_age %>% filter(age_bin == "50+") %>% pull(age), 4),
      age_bin = rep(winners_age %>% filter(age_bin == "50+") %>% pull(age_bin), 4),
      med_age_compare = rep(winners_age %>% filter(age_bin == "50+") %>% pull(med_age_compare), 4),
      series = rep(winners_age %>% filter(age_bin == "50+") %>% pull(series), 4),
      
      x = c(rep(8, nrow(winners_age %>% filter(age_bin == "50+"))), 
            rep(8.5, nrow(winners_age %>% filter(age_bin == "50+"))),
            rep(10, nrow(winners_age %>% filter(age_bin == "50+"))), 
            rep(10, nrow(winners_age %>% filter(age_bin == "50+")))),
      
      y = c(rep(12, nrow(winners_age %>% filter(age_bin == "50+"))), 
            rep(12, nrow(winners_age %>% filter(age_bin == "50+"))),
            rep(9.5, nrow(winners_age %>% filter(age_bin == "50+"))), 
            rep(9, nrow(winners_age %>% filter(age_bin == "50+")))),
      
      line = rep(4, nrow(winners_age %>% filter(age_bin == "50+")) * 4)))
  

#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
title_font <- "Montserrat"
font_color <- "#1A1A1A" # gray
line_color <- "#666666" # lighter gray
bcolor <- "#B0C1D9"

#### Plot ####

# What does it take to win bakeoff?

ggplot(data = winners,
       mapping = aes(x = episode,
                     y = technical)) +
  # Shading for cake layers
  geom_rect(mapping = aes(xmin = 1, xmax = 10,
                          ymin = 1, ymax = 3),
            color = NA,
            fill = "#590219",
            alpha = 0.5) +
  geom_rect(mapping = aes(xmin = 1, xmax = 10,
                          ymin = 3, ymax = 5),
            color = NA,
            fill = "#73022C",
            alpha = 0.5) +
  geom_rect(mapping = aes(xmin = 1, xmax = 10,
                          ymin = 5, ymax = 12),
            color = NA,
            fill = "#D982B2",
            alpha = 0.5) +
  # Technical Standing
  geom_bump(size = 0.3,
            alpha = 0.8,
            color = "#F2F2F2") +
  geom_point(data = winners %>% filter(!star_baker),
             size = 0.5,
             color = "#F2F2F2") + 
  # Star Baker
  geom_point(data = winners %>% filter(star_baker),
             mapping = aes(shape = star_baker),
             size = 6,
             color = "#F2F2F2") +
  # Age bin lines
  geom_polygon(data = lines %>% filter(line == 1),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  geom_polygon(data = lines %>% filter(line == 2),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  geom_polygon(data = lines %>% filter(line == 3),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  geom_polygon(data = lines %>% filter(line == 4),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  # Plot formatting
  scale_y_reverse(breaks = c(1, 3, 5, 12),
                  labels = c("First in Technical", "Third", "Fifth", "Twelfth")) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_shape_manual(values = c("\u2605"),
                     limits = c(TRUE),
                     labels = c("Star Baker")) +
  guides(shape = "none") +
  facet_wrap(~ reorder(baker_label, series),
             scale = "free_x",
             nrow = 2) +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  labs(title = "What does it take to win the Great British Bake Off?",
       subtitle = "Winners tend to place at least fifth in the technical challenge throughout the series. A star indicates the winner earned Star Baker that\nepisode. Winners do not earn Star Baker very frequently. Lines indicate the winner's age: 1 line 20-29, 2 lines 30-39, 3 lines 40-49, and 4\nlines over 50. The median age of all bakers is 34. The median age of winners is 31, and most winners are in their 30s.",
       caption  = "Series 1 and 2 had only 6 and 8 episodes, respectively. All other series had 10 episodes.\nData: {bakeoff} package | Design: Jenn Schilling",
       x = "Episodes →",
       shape = "") +
  theme_bw(base_family = font) +
  theme(axis.line.y = element_blank(), 
        axis.line.x = element_line(color = "#59514A", size = 0.5),
        axis.ticks = element_blank(), 
        axis.text.y = element_text(size = 12, color = font_color, family = font,
                                   hjust = 1),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 12, color = font_color, family = font,
                                    hjust = 0),
        axis.title.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(size = 14, color = font_color, family = font, 
                                  hjust = -0.3, face = "bold"),
        
        panel.grid = element_blank(),
        panel.border = element_blank(),
        
        plot.background = element_rect(fill = bcolor, color = NA),
        panel.background = element_rect(fill = bcolor, color = NA),
        
        legend.position = "top",
        legend.text = element_text(color = font_color, family = font),
        legend.background = element_blank(),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 35, color = font_color,
                                  lineheight = 0.3, face = "bold", family = font),
        plot.subtitle = element_text(size = 16, color = font_color,
                                     lineheight = 0.3, family = font),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 12, color = font_color, 
                                    lineheight = 0.3, hjust = 1, family = font),
        
        plot.margin = margin(10, 10, 10, 10))
  


#### Save ####

ggsave(here("2022-10-25", "bakeoff.png"),
       plot = last_plot(),
       device = "png",
       width = 5,
       height = 3)


#### Make Instagram Version ####


# Title/First Image

ggplot(data = winners,
       mapping = aes(x = episode,
                     y = technical)) +
  # Shading for cake layers
  geom_rect(mapping = aes(xmin = 1, xmax = 10,
                          ymin = 1, ymax = 3),
            color = NA,
            fill = "#590219",
            alpha = 0.5) +
  geom_rect(mapping = aes(xmin = 1, xmax = 10,
                          ymin = 3, ymax = 5),
            color = NA,
            fill = "#73022C",
            alpha = 0.5) +
  geom_rect(mapping = aes(xmin = 1, xmax = 10,
                          ymin = 5, ymax = 12),
            color = NA,
            fill = "#D982B2",
            alpha = 0.5) +
  # Technical Standing
  geom_bump(size = 0.3,
            alpha = 0.8,
            color = "#F2F2F2") +
  geom_point(data = winners %>% filter(!star_baker),
             size = 0.5,
             color = "#F2F2F2") + 
  # Star Baker
  geom_point(data = winners %>% filter(star_baker),
             mapping = aes(shape = star_baker),
             size = 6,
             color = "#F2F2F2") +
  # Age bin lines
  geom_polygon(data = lines %>% filter(line == 1),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  geom_polygon(data = lines %>% filter(line == 2),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  geom_polygon(data = lines %>% filter(line == 3),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  geom_polygon(data = lines %>% filter(line == 4),
               mapping = aes(x = x,
                             y = y),
               fill = "#F2F2F2") +
  # Plot formatting
  scale_y_reverse(breaks = c(1, 3, 5, 12),
                  labels = c("First in Technical", "Third", "Fifth", "Twelfth")) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_shape_manual(values = c("\u2605"),
                     limits = c(TRUE),
                     labels = c("Star Baker")) +
  guides(shape = "none") +
  facet_wrap(~ reorder(baker_label, series),
             scale = "free_x",
             nrow = 2) +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  labs(title = "\nWhat does it take to win the Great\nBritish Bake Off?\n",
       subtitle = "Winners tend to place at least fifth in the technical challenge throughout\nthe series. A star indicates the winner earned Star Baker that episode.\nWinners do not earn Star Baker very frequently. Lines indicate age:\n1 line 20-29, 2 lines 30-39, 3 lines 40-49, and 4 lines over 50. The median\nage of all bakers is 34. The median age of winners is 31, and most\nwinners are in their 30s.\n",
       caption  = "\nSeries 1 and 2 had only 6 and 8 episodes, respectively. All other series had 10 episodes.\nData: {bakeoff} package | Design: Jenn Schilling",
       x = "Episodes →",
       shape = "") +
  theme_bw(base_family = font) +
  theme(axis.line.y = element_blank(), 
        axis.line.x = element_line(color = "#59514A", size = 0.5),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(size = 14, color = font_color, family = font, 
                                  hjust = 0.5, face = "bold"),
        
        panel.grid = element_blank(),
        panel.border = element_blank(),
        
        plot.background = element_rect(fill = bcolor, color = NA),
        panel.background = element_rect(fill = bcolor, color = NA),
        
        legend.position = "top",
        legend.text = element_text(color = font_color, family = font),
        legend.background = element_blank(),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 40, color = font_color,
                                  lineheight = 0.3, face = "bold", family = font),
        plot.subtitle = element_text(size = 23, color = font_color,
                                     lineheight = 0.4, family = font),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 12, color = font_color, 
                                    lineheight = 0.3, hjust = 1, family = font),
        
        plot.margin = margin(10, 10, 10, 10))

ggsave(here("2022-10-25", "bakeoff_1.png"),
       plot = last_plot(),
       device = "png",
       width = 4,
       height = 4)

# Make sub-plots for groups of winners

make_plots <- function(s1, s2){
  
  plot_dat <- winners %>% filter(series >= s1 & series <= s2)
  line_dat <- lines %>% filter(series >= s1 & series <= s2)
  
  if(s2 == 10){
    cap = ""
  }else{
    cap = "\n\n\nData: {bakeoff} package | Design: Jenn Schilling"
  }
  
  p <- ggplot(data = plot_dat,
         mapping = aes(x = episode,
                       y = technical)) +
    # Shading for cake layers
    geom_rect(mapping = aes(xmin = 1, xmax = 10,
                            ymin = 1, ymax = 3),
              color = NA,
              fill = "#590219",
              alpha = 0.5) +
    geom_rect(mapping = aes(xmin = 1, xmax = 10,
                            ymin = 3, ymax = 5),
              color = NA,
              fill = "#73022C",
              alpha = 0.5) +
    geom_rect(mapping = aes(xmin = 1, xmax = 10,
                            ymin = 5, ymax = 12),
              color = NA,
              fill = "#D982B2",
              alpha = 0.5) +
    # Technical Standing
    geom_bump(size = 1,
              alpha = 0.8,
              color = "#F2F2F2") +
    geom_point(data = plot_dat %>% filter(!star_baker),
               size = 1.5,
               color = "#F2F2F2") + 
    # Star Baker
    geom_point(data = plot_dat %>% filter(star_baker),
               mapping = aes(shape = star_baker),
               size = 12,
               color = "#F2F2F2") +
    # Age bin lines
    geom_polygon(data = line_dat %>% filter(line == 1),
                 mapping = aes(x = x,
                               y = y),
                 fill = "#F2F2F2") +
    geom_polygon(data = line_dat %>% filter(line == 2),
                 mapping = aes(x = x,
                               y = y),
                 fill = "#F2F2F2") +
    geom_polygon(data = line_dat %>% filter(line == 3),
                 mapping = aes(x = x,
                               y = y),
                 fill = "#F2F2F2") +
    geom_polygon(data = line_dat %>% filter(line == 4),
                 mapping = aes(x = x,
                               y = y),
                 fill = "#F2F2F2") +
    # Plot formatting
    scale_y_reverse(breaks = c(1, 3, 5, 12),
                    labels = c("First in Technical", "Third", "Fifth", "Twelfth")) +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    scale_shape_manual(values = c("\u2605"),
                       limits = c(TRUE),
                       labels = c("Star Baker")) +
    guides(shape = "none") +
    facet_wrap(~ reorder(baker_label, series),
               scale = "free_x",
               ncol = 2,
               nrow = 2,
               as.table = TRUE) +
    coord_cartesian(clip = "off",
                    expand = FALSE) +
    labs(subtitle = "\n",
         caption  = cap,
         x = "Episodes →",
         shape = "") +
    theme_bw(base_family = font) +
    theme(axis.line.y = element_blank(), 
          axis.line.x = element_line(color = "#59514A", size = 0.5),
          axis.ticks = element_blank(), 
          axis.text.y = element_text(size = 18, color = font_color, family = font,
                                     hjust = 1),
          axis.text.x = element_blank(),
          axis.title.x = element_text(size = 18, color = font_color, family = font,
                                      hjust = 0),
          axis.title.y = element_blank(),
          
          strip.background = element_blank(),
          strip.text = element_text(size = 24, color = font_color, family = font, 
                                    hjust = -0.1, face = "bold"),
          
          panel.grid = element_blank(),
          panel.border = element_blank(),
          
          plot.background = element_rect(fill = bcolor, color = NA),
          panel.background = element_rect(fill = bcolor, color = NA),
          
          legend.position = "top",
          legend.text = element_text(color = font_color, family = font),
          legend.background = element_blank(),
          
          plot.title.position = "plot",
          plot.title = element_text(size = 35, color = font_color,
                                    lineheight = 0.3, face = "bold", family = font),
          plot.subtitle = element_text(size = 16, color = font_color,
                                       lineheight = 0.3, family = font),
          plot.caption.position = "plot",
          plot.caption = element_text(size = 14, color = font_color, 
                                      lineheight = 0.3, hjust = 1, family = font),
          
          plot.margin = margin(10, 10, 10, 10))
  
  if(s2 == 10){
    
    p2 <- ggplot() +
      theme_void() +
      labs(caption = "\n\n\nData: {bakeoff} package | Design: Jenn Schilling") +
      theme(plot.background = element_rect(fill = bcolor, color = NA),
            panel.background = element_rect(fill = bcolor, color = NA),
            panel.border = element_blank(),
            plot.caption.position = "plot",
            plot.caption = element_text(size = 14, color = font_color, 
                                        lineheight = 0.3, hjust = 1, family = font),
            plot.margin = margin(10, 10, 10, 10))
    
    p + p2 + 
      plot_layout(nrow = 2, heights = c(1.5, 1)) +
      plot_annotation(theme = theme(plot.background = element_rect(fill = bcolor, color = NA),
                                    panel.background = element_rect(fill = bcolor, color = NA),
                                    panel.border = element_blank(),
                                    plot.caption.position = "plot",
                                    plot.caption = element_text(size = 14, color = font_color, 
                                                                lineheight = 0.3, hjust = 1, family = font),
                                    plot.margin = margin(0, 0, 0, 0)))
    
    
  }else{
    p
  }
  
  
}


make_plots(1, 4)

ggsave(here("2022-10-25", "bakeoff_2.png"),
       plot = last_plot(),
       device = "png",
       width = 4,
       height = 4)

make_plots(5, 8)

ggsave(here("2022-10-25", "bakeoff_3.png"),
       plot = last_plot(),
       device = "png",
       width = 4,
       height = 4)

make_plots(9, 10)

ggsave(here("2022-10-25", "bakeoff_4.png"),
       plot = last_plot(),
       device = "png",
       width = 4,
       height = 4)

