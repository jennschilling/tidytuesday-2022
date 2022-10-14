# Author : Jenn Schilling
# Title: #TidyTuesday Product Hunt
# Date: Oct 4 2022

#### Libraries ####

library(here)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(tidytext)
library(ggraph)
library(igraph)
library(widyr)


#### Get the Data ####

# Read in the data
product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')


# Get the individual categories
product_hunt_sep <- product_hunt %>%
  mutate(category_tags = str_replace_all(category_tags, "['\\[\\]]", "")) %>%
  separate_rows(category_tags, sep = ", ")

# Get top categories
top_cat <- product_hunt_sep %>%
  count(category_tags) %>%
  top_n(10) %>%
  mutate(category_tags = str_replace_all(category_tags, " ", ""),
         category_tags = str_to_lower(category_tags),
         category_tags = str_replace_all(category_tags, "['-/\\[\\]]", "")) %>%
  pull(category_tags)

# Get category count
cat_count <- product_hunt_sep %>%
  count(category_tags) %>%
  mutate(category_tags_join = str_replace_all(category_tags, " ", ""),
         category_tags_join = str_to_lower(category_tags_join),
         category_tags_join = str_replace_all(category_tags_join, "['-/\\[\\]]", ""),
         category_tags = str_to_title(category_tags)) %>%
  rename(total_n = n,
         category_tags_label = category_tags)

# Reference: https://www.tidytextmining.com/ngrams.html#visualizing-a-network-of-bigrams-with-ggraph
# Reference: https://www.tidytextmining.com/nasa.html

# Get categories that occur together
product_hunt_categories <- product_hunt %>%
  select(id, category_tags) %>%
  mutate(category_tags = str_replace_all(category_tags, " ", ""), # push categories with a space in them together
         category_tags = str_replace_all(category_tags, ",", ", "), # add a space after each category so they can be separated into bigrams
         category_tags = str_replace_all(category_tags, "['-/\\[\\]]", "")) %>%
  unnest_tokens(category, category_tags) %>%
  pairwise_count(category, id, sort = TRUE, upper = FALSE)

# Filter and get regular case labels
product_hunt_categories_filtered <- product_hunt_categories %>%
  filter((item1 %in% top_cat | item2 %in% top_cat) & n >= 200) %>%
  left_join(cat_count, by = c("item1" = "category_tags_join")) %>%
  rename(word1_n = total_n,
         word1_label = category_tags_label) %>%
  left_join(cat_count, by = c("item2" = "category_tags_join")) %>%
  rename(word2_n = total_n,
         word2_label = category_tags_label) %>%
  select(word1_label, word2_label, n)

# Get counts total counts for each category to size labels by count
# Reference: https://stackoverflow.com/questions/53219303/ggraph-increase-node-size-based-on-frequency
# Reference: https://stackoverflow.com/questions/46332738/adding-word-count-size-as-a-layer-to-the-node-size-on-a-cooccurrence-network-cha

filtered_words <- product_hunt_categories_filtered %>%
  pivot_longer(word1_label:word2_label) %>%
  distinct(value) %>%
  pull(value)

category_counts <- cat_count %>%
  select(category_tags_label, total_n) %>%
  rename(category = category_tags_label) %>%
  filter(category %in% filtered_words)
  

# Make graph
product_hunt_categories_graph <- product_hunt_categories_filtered %>%
  graph_from_data_frame(vertices = category_counts)


#### Formatting ####

# Font
font_add_google(name = "Montserrat")
showtext_auto()

# Theme Setup
font <- "Montserrat"
title_font <- "Montserrat"
font_color <- "#F2F2F2" 
bcolor <- "#262626"

#### Plot ####

set.seed(7275)

ggraph(product_hunt_categories_graph, 
       layout = "fr") +
  geom_edge_link(aes(edge_alpha = n,
                     edge_width = n), 
                 show.legend = FALSE,
                 color = "#648C81",
                 lineend = "round") +
  geom_node_text(aes(label = str_replace_all(name, " ", "\n"),
                     size = total_n), 
                 family = font,
                 color = font_color,
                 vjust = 0.5, 
                 hjust = 0.5,
                 show.legend = FALSE,
                 lineheight = 0.3) +
  scale_size_continuous(range = c(6, 16)) +
  labs(title = "A network graph to explore the questions: which product categories occur most\noften and which categories occur together on Product Hunt?",
       caption = "Data: components.one by way of Data is Plural | Design: Jenn Schilling") +
  theme_void() +
  theme(panel.background = element_rect(fill = bcolor, colour = NA),
        plot.background = element_rect(fill = bcolor, colour = NA),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 50, color = font_color, family = title_font, lineheight = 0.3, face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 20, color = font_color, lineheight = 0.3, hjust = 1, family = title_font),
       
        plot.margin = margin(10, 10, 10, 10))


#### Save ####

ggsave(here("2022-10-04", "product_hunt_network.png"),
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 7)



