###############################################################
#   T E X T   &   S E N T I M E N T   A N A L Y S I S  -  2   #
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

#########################################################################################
#   P U B L I K A T I O N E N   V O N   J A N E   A U S T E N   V O R B E R E I T E N   #
#########################################################################################

# Zugriff auf die Publikationen von Jane Austen
install.packages("janeaustenr")
library(janeaustenr)

# Publikationen als Tidy
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Wörter ohne interpretativen Gehalt herausfiltern
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

###################################################################################
#   W Ö R T E R   E I N E S   S E N T I M E N T S   I D E N T I F I Z I E R E N   #
###################################################################################

# Sprachbibliotheken laden
install.packages("textdata")
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# Sprachbibliothek und Sentiment auswählen
nrc_fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")                  # Mehr Sentiments: https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

# Ausgewähltes Sentiment in "Stolz und Vorurteil"
tidy_books %>%
  filter(book == "Pride & Prejudice") %>%     # Übersicht über Publikationen: summary(tidy_books)
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

#####################################################################################
#   S E N T I M E N T   D E R   P U B L I K A T I O N E N   V E R G L E I C H E N   #
#####################################################################################

# Sentiment Analyse für alle Publikationen
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(book, index = linenumber %/% 100, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Ergebnis visualisieren
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) + facet_wrap(~book, ncol = 2, scales = "free_x")

###################################################################
#   S P R A C H B I B L I O T H E K E N   V E R G L E I C H E N   #
###################################################################

# "Emma" als Publikation auswählen
emma <- tidy_books %>% 
  filter(book == "Emma")

# 1. Sprachbbliothek anwenden
afinn <- emma %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 100) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# 2. und 3. Sprachbibliothek anwenden
bing_and_nrc <- bind_rows(
  emma %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  emma %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))) %>%
  mutate(method = "NRC")) %>%
    count(method, index = linenumber %/% 100, sentiment) %>%
    pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
   mutate(sentiment = positive - negative)

# Vergleich der Sprachbibliotheken visualisieren
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

###############################################################
#   P O S I T I V E   U N D   N E G A T I V E   W Ö R T E R   #
###############################################################

# Positive und negative Wörter auflisten
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Positive und negative Wörter visualisieren
bing_word_counts %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "sentiment")

#####################################################################
#  W Ö R T E R   V O N   A N A L Y S E   A U S S C H L I E S S E N  #
#####################################################################

# Individuelle Wörter ohne interpretativen Gehalt definieren
custom_stop_words <- c("miss")
custom_stop_words <- tibble(1:1, word=custom_stop_words)

# Individuelle Wörter ohne interpretativen Gehalt herausfiltern (hier: "miss")
tidy_books <- tidy_books %>%
  anti_join(custom_stop_words)