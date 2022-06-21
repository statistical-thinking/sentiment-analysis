###############################################################
#   T E X T   &   S E N T I M E N T   A N A L Y S I S  -  1   #
###############################################################

#######################
#   P A C K A G E S   #
#######################

# Packages laden (ggfls. zuvor installieren)
library(dplyr)        # Datenverarbeitung
library(stringr)      # Textverarbeitung
library(tidytext)     # Textverarbeitung
library(tidyr)        # Textverarbeitung
library(ggplot2)      # Visualisierung
library(igraph)       # Visualisierung
library(ggraph)       # Visualisierung

#################################################
#   E I N F Ü H R U N G :   T I D Y   T E X T   #
#################################################

# Text anlegen
text <- c("Der Weltraum, unendliche Weiten.",
          "Wir schreiben das Jahr 2200.",
          "Dies sind die Abenteuer des Raumschiffs Enterprise",
          "Das mit seiner 400 Mann starken Besatzung fünf Jahre lang unterwegs ist,",
          "um neue Welten zu erforschen, neues Leben und neue Zivilisationen.",
          "Viele Lichtjahre von der Erde entfernt dringt die Enterprise in Galaxien vor,",
          "die nie ein Mensch zuvor gesehen hat.")
text

# Text als Tibble
text_df <- tibble(line=1:7, text=text)
text_df

# Text als Tidy
text_df %>%
  unnest_tokens(word, text)

#############################################################
#   W O R T N E T Z W E R K E   V I S U A L I S I E R E N   #
#############################################################

# 1. Funktion für Wortpaare definieren
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

# 2. Funktion zum Visualisieren von Wortpaaren definieren
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "open", length = unit(.10, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "green", size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# 1. Funktion auf "Star Trek Intro" anwenden
text_df_bigrams <- text_df %>%
  count_bigrams()

# 2. Funktion auf "Star Trek Intro" anwenden
text_df_bigrams %>%
  filter(n > 0,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()