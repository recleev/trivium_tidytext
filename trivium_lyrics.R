
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(geniusR)
library(rebus)
library(tidytext)
library(drlib)
library(quanteda)
library(stm)
library(wordcloud)
library(extrafont)

# Get Silence in the Snow Album -------------------------------------------

silence <- 
  genius_tracklist("Trivium",
                   "Silence in the snow") %>% 
  filter(title != "Snøfall") %>% 
  mutate(lyrics = map(title,
                      genius_lyrics,
                      artist = "Trivium"),
         album = "Silence in the snow") %>% 
  unnest()

# Create Get Trivium Album Function ---------------------------------------

trivium_albums <- 
  c("Ember to Inferno",
    "Ascendancy",
    "The Crusade",
    "Shogun",
    "In Waves",
    "Vengeance Falls",
    "Silence in the Snow",
    "The Sin and the Sentence")

get_trivium_album <- function(trivium_album) {
  genius_tracklist("Trivium",
                   trivium_album) %>% 
    mutate(lyrics = map(title,
                        genius_lyrics,
                        artist = "Trivium"),
           album = trivium_album) %>% 
    unnest()
}

trivium_incomplete <- 
  map_dfr(trivium_albums,
          possibly(get_trivium_album,
                   tibble(title = NA,
                          track_n = NA,
                          album = NA,
                          text = NA,
                          line = NA))) %>% 
  drop_na()

# Combine All Trivium Albums ----------------------------------------------

trivium_covers <- paste(c("Master Of Puppets", 
                          "Iron Maiden", 
                          "Slave New World", 
                          "Skulls... We Are 138", 
                          "Losing My Religion"),
                         collapse = "|")
trivium <-
  trivium_incomplete %>% 
  bind_rows(silence) %>%
  filter(str_detect(title,
                    trivium_covers) == FALSE)

# Clean Lyrics ------------------------------------------------------------

SMART_stop_words <- 
  stop_words %>% 
  filter(lexicon == "SMART")

apostrophe <- paste(c("'" %R% one_or_more(ALPHA),
                      "’" %R% one_or_more(ALPHA)),
                    collapse = "|")

clean_trivium_lyrics <- 
  trivium %>% 
  mutate(text = str_replace_all(text,
                                "asun-der",
                                "asunder")) %>% 
  unnest_tokens(lyrics, text) %>%
  mutate(lyrics = str_replace(lyrics,
                              apostrophe,
                              ""),
         album = factor(album,
                        levels = trivium_albums),
         album = str_to_title(album)) %>%
  anti_join(SMART_stop_words,
            by = c("lyrics" = "word"))

clean_trivium_lyrics %>% 
  count(lyrics, sort = TRUE) %>% 
  filter(str_detect(lyrics,
                    "i’m|it’s|don’t|i’ll|i’ve|that’s|they’re"))

clean_trivium_lyrics %>% 
  filter(is.na(album)) %>% 
  count(title)

# Create Trivium Palette, Theme, and Functions ----------------------------

trivium_palette <- c("yellow2", # Ember
                     "orange2", # Ascendancy
                     "royalblue4", # Crusade
                     "darkred", # Shogun
                     "darkgrey", # Waves
                     "steelblue4", # Vengeance
                     "whitesmoke", # Silence
                     "saddlebrown") # Sin

theme_trivium <- function(...) {
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_blank(),
        text = element_text(family = "Georgia",
                            color = "grey"),
        axis.text = element_text(color = "grey"),
        strip.text = element_text(color = "grey"),
        ...)
}

scale_fill_trivium <- function(...) {
  scale_fill_manual(...,
                    values = trivium_palette)
}

trivium_album_col <- 
  trivium_albums %>% 
  length() %>% 
  sqrt() %>% 
  floor()

facet_trivium_albums <- function(album, trivium_albums, ...) {
  facet_wrap(~factor(album,
                     levels = str_to_title(trivium_albums)),
             scales = "free_y",
             ncol = trivium_album_col,
             ...)
}

# Top Words per Album -----------------------------------------------------

clean_trivium_lyrics %>% 
  count(album, lyrics, sort = TRUE) %>%
  group_by(album) %>%
  top_n(10, n) %>%
  ggplot(aes(reorder_within(lyrics,
                            n,
                            within = album),
             n,
             fill = factor(album,
                           levels = str_to_title(trivium_albums)))) +
  scale_x_reordered() +
  geom_col(show.legend = FALSE) +
  facet_trivium_albums() +
  coord_flip() +
  labs(title = "Most Common Words in Every Trivium Album",
       y = "Frequency",
       x = "Word") +
  theme_trivium() +
  scale_fill_trivium()

trivim_stm <- 
  trivium_dfm %>% 
  stm(K = 8, verbose = FALSE, init.type = "Spectral")

trivium_beta <- 
  trivim_stm %>% 
  tidy()

trivium_beta %>% 
  group_by(topic) %>% 
  top_n(10, beta)

# Topic Model -------------------------------------------------------------

trivium_dfm <- 
  clean_trivium_lyrics %>% 
  count(album, lyrics, sort = TRUE) %>% 
  cast_dfm(album, lyrics, n)

# Term Frequency and Inverse Document Frequency of Words per Album --------

clean_trivium_lyrics %>% 
  count(album, lyrics) %>% 
  bind_tf_idf(lyrics,
              album,
              n) %>% 
  group_by(album) %>% 
  top_n(10, tf_idf) %>%
  ggplot(aes(reorder_within(lyrics,
                            tf_idf,
                            album),
         tf_idf,
         fill = factor(album,
                       levels = str_to_title(trivium_albums)))) +
  scale_x_reordered() +
  geom_col(show.legend = FALSE) +
  facet_trivium_albums() +
  coord_flip() +
  labs(title = "TF-IDF of Words in Every Trivium Album",
       y = "TF-IDF",
       x = "Word") +
  theme_trivium() +
  scale_fill_trivium()

clean_trivium_lyrics %>% 
  filter(album == "Ascendancy") %>% 
  count(title, lyrics, sort = TRUE) %>% 
  View()

# Proportion of Negative and Positive Bing Words per Album ----------------

sentiments %>% 
  count(lexicon, sentiment) %>% 
  View()

bing <- 
  sentiments %>% 
  filter(lexicon == "bing") %>% 
  select(-score)

trivium_bing <- 
  clean_trivium_lyrics %>% 
  inner_join(bing,
             by = c("lyrics" = "word"))

trivium_bing %>% 
  count(lyrics, sentiment, sort = TRUE) %>% 
  View()

trivium_bing %>% 
  count(album,
        sentiment) %>% 
  group_by(album) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(factor(album,
                    levels = trivium_albums),
             prop,
             fill = factor(sentiment,
                           levels = c("positive",
                                      "negative")))) +
  geom_col(position = "dodge") +
  labs(title = "Proportion of Positive and Negative Words in Every Trivium Album",
       subtitle = "Based in Bing Liu's Sentiment Lexicon",
       x = "Album",
       y = "Proportion of Words",
       fill = "Sentiment") +
  scale_y_percent() +
  theme_trivium() +
  scale_fill_manual(values = c("steelblue",
                               "darkred"))

# Trivium WordCloud of Positive and Negative Words ------------------------

par(bg = "black")
trivium_bing %>% 
  count(lyrics, sentiment, sort = TRUE) %>%
  with(wordcloud(lyrics,
                 n,
                 scale = c(5, 0.25),
                 ordered.colors = TRUE,
                 random.order = FALSE,
                 rot.per = 0,
                 colors = c("darkred", "steelblue4")[factor(sentiment)],
                 max.words = 100,
                 fixed.asp = FALSE))

# Total AFINN Score of Trivium Albums -------------------------------------

afinn <- 
  sentiments %>% 
  filter(lexicon == "AFINN") %>% 
  select(-sentiment)

trivium_afinn <- 
  clean_trivium_lyrics %>% 
  inner_join(afinn,
             by = c("lyrics" = "word")) 

trivium_afinn %>% 
  group_by(album) %>% 
  summarise(total_score = sum(score,
                              na.rm = TRUE)) %>%
  ggplot(aes(album,
             total_score)) +
  geom_col(fill = "darkred") +
  labs(title = "Total AFINN Score of Trivium Albums",
       x = "Album",
       y = "AFINN Score") +
  theme_trivium()


# Dripping Blood AFINN Scores Histogram -----------------------------------

trivium_afinn %>% 
  ggplot(aes(score)) +
  geom_density(fill = "darkred") +
  scale_y_reverse() +
  theme_trivium() +
  geom_hline(yintercept = 0, color = "darkred", size = 1) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

# Top Five Most Negative Trivium Songs per Album --------------------------

trivium_afinn %>% 
  group_by(album, title) %>%
  summarise(total_score = sum(score,
                              na.rm = TRUE)) %>% 
  top_n(5, -total_score) %>% 
  ggplot(aes(reorder_within(title,
                            -total_score,
                            album),
             -total_score,
             fill = factor(album,
                           levels = trivium_albums))) +
  scale_x_reordered() +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_trivium_albums() +
  labs(title = "Top Five Most Negative Trivum Songs per Album",
       subtitle = "Based on AFINN Scores",
       x = "Song Title",
       y = "Negative AFINN Score") +
  theme_trivium() +
  scale_fill_trivium()

trivium_afinn %>% 
  group_by(album) %>% 
  summarise(total_score = sum(score)) %>% 
  mutate(mean_total_score = mean(total_score))

# Eight Basic Emotions in Trivium Albums ----------------------------------

nrc <- 
  sentiments %>% 
  filter(lexicon == "nrc" &
           !(sentiment %in% c("positive", "negative")))

trivium_nrc <- 
  clean_trivium_lyrics %>% 
  inner_join(nrc,
             by = c("lyrics" = "word")) %>% 
  select(-score)

trivium_nrc %>% 
  count(album, sentiment,
        sort = TRUE) %>% 
  ggplot(aes(reorder_within(sentiment,
                            n,
                            album),
             n,
             fill = factor(album,
                           levels = str_to_title(trivium_albums)))) +
  scale_x_reordered() +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_trivium_albums() +
  theme_trivium() +
  scale_fill_trivium()

# Top Five Songs Per Sentiment --------------------------------------------

trivium_nrc %>% 
  count(title, 
        sentiment,
        sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(5, n) %>% 
  ggplot(aes(reorder_within(title,
                            n,
                            sentiment),
             n,
             fill = sentiment)) +
  scale_x_reordered() +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment,
             ncol = 2,
             scales = "free_y") +
  theme_trivium() +
  scale_fill_trivium()

