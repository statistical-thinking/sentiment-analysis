########################################################
#   T E X T   &   S E N T I M E N T   A N A L Y S I S  #
########################################################

#######################
#   P A C K A G E S   #
#######################

# Packages laden (ggfls. zuvor installieren)
library(dplyr)        # Datenverarbeitung
library(stringr)      # Textverarbeitung
library(tidytext)     # Textverarbeitung
library(ggplot2)      # Visualisierung
library(gridExtra)    # Visualisierung

###################################
#   V O R B E R E I T U N G E N   #
###################################

# Text in R einlesen
imported_text <- read.delim("your_unformatted_document.txt", header=F, sep="\t")
dim(imported_text) # line=1:N

# Text als Tibble
text_df <- tibble(line=1:10, text=imported_text$V1)
text_df

# Text als Tidy
text_tidy <- text_df %>%
  unnest_tokens(word, text)

# Wörter ohne interpretativen Gehalt herausfiltern
data(stop_words)
text_tidy <- text_tidy %>%
  anti_join(stop_words)

###################################################################
#   H Ä U F I G S T E   W Ö R T E R   U N D   S E N T I M E N T   #
###################################################################

# Positive und negative Wörter auflisten
nrc_word_counts <- text_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Häufigste Wörter und deren Sentiment
plot1 <- nrc_word_counts %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "sentiment (n)") +
  ggtitle("Common Words & Sentiments (Frequency)")

############################################
#  A N Z A H L  A N   S E N T I M E N T S  #
############################################

# Absolute Sentiments auflisten
plot2 <- nrc_word_counts %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  ggtitle("Sentiments (Distribution)") +
  coord_flip()

#############################################################
#   S E N T I M E N T E I N S A T Z   I M   V E R L A U F   #
#############################################################

# Verlauf des Sentiments auswerten
nrc_word_counts <- text_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(line, sentiment, sort = TRUE) %>%
  ungroup()
nrc_word_counts

# Verlauf des Sentiments darstellen
plot3 <- ggplot(data = nrc_word_counts, mapping = aes(x = line, y = n)) +
  geom_smooth() + 
  xlab("document (line)") + 
  ylab("sentiment (conditional mean)") +
  ggtitle("Intertemporal Use of Sentiments (Conditional Mean)")

#####################################
#  S E N T I M E N T V E R L A U F  #
#####################################

# Absolutes Sentiment auswerten
bing_word_counts <- bind_rows(
  text_tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."))
bing_word_counts

# Absolutes Sentiment in Summenscore überführen
sentiment_sum <- ifelse(bing_word_counts$sentiment == "positive", 1, -1)
sentiment_sum_df <- cbind(bing_word_counts$line, sentiment_sum)
colnames(sentiment_sum_df) <- c('var1', 'var2')
sentiment_sum_df <- as.data.frame(sentiment_sum_df)
sentiment_sum_df <- aggregate(sentiment_sum_df$var2, by=list(line=sentiment_sum_df$var1), FUN=sum)

# Verlauf des Sentiments darstellen
plot4 <- ggplot(data = sentiment_sum_df, mapping = aes(x = line, y = x)) + 
  geom_smooth() + 
  xlab("document (line)") + 
  ylab("sentiment (score)") +
  ggtitle("Intertemporal Use of Sentiments (Score)")

#####################################
#  G E S A M T A U S W E R T U N G  #
#####################################

grid.arrange(plot1, plot2, plot3, plot4)
