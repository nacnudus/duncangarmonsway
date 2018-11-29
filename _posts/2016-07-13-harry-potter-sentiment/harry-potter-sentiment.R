# Do http://juliasilge.com/blog/Life-Changing-Magic/ on Harry Potter.

# Requires Calibre to be installed, with the command-line tool `ebook-convert`.

library(dplyr)
library(dtplyr)
library(purrr)
library(tidyr)
library(tibble)
library(tidytext) # unnest_tokens
library(stringi)
library(tokenizers) # stopwords, tokenize_sentences, tokenize_ngrams
library(syuzhet) # get_transformed_values
library(ggplot2)
library(directlabels)
library(here)

# Convert epub to txt using Calibre at the command line ========================

# Once-only

# system("ebook-convert epub/HP1_Philosophers_Stone_en-gb.epub   txt/1-PS.txt")
# system("ebook-convert epub/HP2_Chamber_of_Secrets_en-gb.epub   txt/2-CS.txt")
# system("ebook-convert epub/HP3_Prisoner_of_Azkaban_en-gb.epub  txt/3-PA.txt")
# system("ebook-convert epub/HP4_Goblet_of_Fire_en-gb.epub       txt/4-GF.txt")
# system("ebook-convert epub/HP5_Order_of_the_Phoenix_en-gb.epub txt/5-OP.txt")
# system("ebook-convert epub/HP6_Half-Blood_Prince_en-gb.epub    txt/6-HP.txt")
# system("ebook-convert epub/HP7_Deathly_Hallows_en-gb.epub      txt/7-DH.txt")

# Load books into a data frame with "title" and "text" columns =================

# Titles as factor in series order
titles <- c("1. Philosopher's Stone",
            "2. Chamber of Secrets",
            "3. Prisoner of Azkaban",
            "4. Goblet of Fire",
            "5. Order of the Phoenix",
            "6. Half-Blood Prince",
            "7. Deathly Hallows")

# Filenames (alphanumerically in series order)
files <-
  list.files(here("data", "txt"), full.names = TRUE) %>%
  normalizePath

# Load the texts (one paragraph per row)
books <-
  tibble(title = titles, file = files) %>%
  mutate(text = map(file, readLines)) %>%
  select(-file) %>%
  unnest %>%
  filter(text != "")

# Mark chapters (not really necessary for this script)
# Start-of-book markers: ^— CHAPTER ONE —$ but with different kinds of -.
# End-of-book markers: ^Titles available.*
books <-
  books %>%
  group_by(title) %>%
  mutate(chapter = stri_extract(text, regex = "^[—–] CHAPTER .*|^Titles available.*")) %>%
  fill(chapter) %>%
  filter(!is.na(chapter)) %>%
  filter(cumsum(stri_detect(chapter, regex = "^Titles available.*")) == 0) %>%
  # Convert chapter headings to integers
  mutate(chapter = stri_extract_last(chapter, regex = "[A-Z]+")) %>%
  mutate(chapter = as.integer(factor(chapter, levels = unique(chapter)))) %>%
  # Drop chapter headings
  group_by(title, chapter) %>%
  mutate(linenumber = row_number()) %>%
  filter(linenumber >= 3) %>%
  ungroup()

# Convert non-ascii characters for tokenizing and ’ to ' to match stopwords
books <-
  books %>%
  mutate(text = stri_replace_all(text, replacement = "'", regex = "[‘’]"),
         text = stri_replace_all(text, replacement = "-", regex = "[–?]"))

# Sentences ====================================================================

# Break into sentences so that ngrams don't cross sentence boundaries
sentences <-
  books %>%
  summarise(sentence = list(unlist(map(text, tokenize_sentences)))) %>%
  unnest

saveRDS(sentences, here("data", "sentences.Rds"))
sentences <- readRDS(here("data", "sentences.Rds"))

# # Plot

sentences <-
  sentences %>%
  count(sentence) %>%
  arrange(desc(n)) %>%
  mutate(rank = min_rank(-n))

sentences %>%
  filter(n >= 2) %>%
  filter(stri_trim(sentence) != "*") %>%
  select(-rank) %>%
  mutate(words = stri_count_words(sentence)) %>%
  group_by(words) %>%
  arrange(desc(n)) %>%
  slice(1:80) %>%
  ungroup %>%
  saveRDS(here("data", "dashboard", "sentences80.Rds"))

# # Slate's list is somewhat edited.  Speech has been ignored -- fair enough --
# # but "He waited." appears only three times, and "Something he didn't have last
# # time." only twice.  "Harry nodded.", not present in Slate's list, tops mine
# # with 14 entries, one more than "Nothing happened.".
# sentences %>%
#   slice(1:30) %>%
#   arrange(desc(rank)) %>%
#   mutate(sentence = factor(sentence, levels = sentence)) %>%
#   ggplot(aes(sentence, n)) +
#   coord_flip() +
#   geom_text(aes(label = sentence), hjust = 0) +
#   scale_y_continuous(limits = c(0, 61), expand = c(0, 0)) +
#   xlab("") +
#   ylab("Occurrences (left-justified)") +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.grid = element_blank()) +
#   ggtitle("Top 30 Harry Potter sentences")
# # The missing sentence is the pause "*"
#
# sentences %>% filter(stri_detect(sentence,
#                                  regex = paste("^Harry nodded.$",
#                                                "^Nothing happened.$",
#                                                "^He waited.$",
#                                                "^Something he didn't have last time.$",
#                                                sep = "|")))

# # Bugged me since primary school
# tokenize_sentences("'Are you going?' Harry asked.")
# tokenize_sentences("Ron asked, 'Are you going?' Harry shrugged.")
# tokenize_sentences("'You should go,' Harry said")
# tokenize_sentences("'Go now.' Harry went.")
# # Were I king, I'd decree the following usage
# tokenize_sentences("Ron asked, 'Are you going?'. Harry shrugged.")
# tokenize_sentences("'You should go.', Harry said")


# n-grams ======================================================================

# Takes a few seconds
words <- 2:10
ngrams <-
  readRDS(here("data", "sentences.Rds")) %>%
  as.list %>%
  .[rep(1, length(words))] %>%
  tibble(sentence = .) %>%
  mutate(words = words) %>%
  rowwise %>%
  # This avoids unnesting, which is slow
  mutate(ngram = list(data_frame(ngram = unlist(tokenize_ngrams(sentence,
                                                                n = words)),
                                 words = words))) %>%
  .$ngram %>%
  bind_rows %>%
  count(words, ngram) %>%
  group_by(words) %>%
  mutate(rank = min_rank(-n)) %>%
  arrange(words, rank)

saveRDS(ngrams, here("data", "ngrams.Rds"))
ngrams <- readRDS(here("data", "ngrams.Rds"))

saveRDS(ngrams %>% filter(rank <= 80), here("data", "dashboard", "ngrams80.Rds"))

# # Is 'turned on [his/her] heel' the most common phrase?
# fourgrams <-
#   ngrams %>%
#   filter(words == 4)
#
# fourgrams %>%
#   filter(stri_detect(ngram, regex = "turn(ed|ing) on h(is|er) heel"))
#
# # 7 + 4 + 1 = 12, rank = 626 at best
# fourgrams %>%
#   filter(n == 12) %>%
#   distinct(rank)

# fourgrams %>%
#   arrange(rank) %>%
#   slice(1:40) %>%
#   arrange(desc(rank)) %>%
#   mutate(ngram = factor(ngram, levels = ngram)) %>%
#   ggplot(aes(ngram, n)) +
#   coord_flip() +
#   geom_text(aes(label = ngram), hjust = 0) +
#   scale_y_continuous(limits = c(0, 275), expand = c(0, 0)) +
#   xlab("") +
#   theme_minimal() +
#   ylab("Occurrences (left-justified)") +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.grid = element_blank()) +
#   ggtitle("Top 40 Harry Potter 4-grams")
#
# ggsave(here("data", "4grams.png"))

# Prepare for sentiment analysis ===============================================

# Convert to one token per row
books <-
  books %>%
  unnest_tokens(word, text) %>%
  group_by(title, chapter, linenumber) %>%
  mutate(wordnumber = row_number()) %>%
  ungroup
j
saveRDS(books, here("data", "books.Rds"))
books <- readRDS(here("data", "books.Rds"))

# Main characters ==============================================================

# # Hermione isn't actually so far behind Ron
# books %>%
#   count(word, sort = TRUE) %>%
#   slice(1:10) %>% as.data.frame

# booksplot <-
#   books %>%
#   ungroup %>%
#   # Count occurrences of name
#   arrange(title, chapter, linenumber, wordnumber) %>%
#   mutate(harry = cumsum(stri_detect(word, regex = "harry")),
#          ron = cumsum(stri_detect(word, regex = "ron")),
#          hermione = cumsum(stri_detect(word, regex = "hermione"))) %>%
#   # Take only the end-of-chapter numbers (fewer data points for slow ggplot2)
#   group_by(title, chapter, linenumber) %>%
#   summarise_at(vars(harry, ron, hermione), last) %>%
#   ungroup %>%
#   # Plot
#   mutate(linenumber = row_number()) %>%
#   gather(character, mentions, harry, ron, hermione) %>%
#   mutate(character = factor(character,
#                             levels = c("harry", "ron", "hermione"),
#                             labels = c("Harry ", "Ron ", "Hermione "))) %>%
#   ggplot(aes(linenumber, mentions)) +
#   geom_line(aes(colour = character)) +
#   scale_colour_discrete("") +
#   xlim(0, 40000) +
#   xlab("Progress through the series") +
#   ylab("Number of mentions of name (the exact name in the legend)") +
#   theme(axis.ticks.x = element_blank(),
#         axis.text.x = element_blank())
# booksplot %>% direct.label(method = "last.polygons")
#
# ggsave(here("data", "trio.png"))

# Remove stopwords
data("stop_words")
books <-
  books %>%
  anti_join(stop_words) %>%
  arrange(title, chapter, linenumber, wordnumber)


# Sentiment analysis continued =================================================

bing <-
  sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, sentiment)

sentiment <-
  books %>%
  group_by(title) %>%
  arrange(chapter, linenumber, wordnumber) %>%
  mutate(linenumber = row_number()) %>%
  inner_join(bing) %>%
  count(title, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ungroup

sentiment_raw <- sentiment # store for later plotting

# sentiment_raw %>%
# ggplot(aes(index, sentiment, fill = title)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   facet_wrap(~title, ncol = 2, scales = "free_x", dir = "v") +
#   ylab("Sentiment (positive minus negative)") +
#   xlab("") +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.grid = element_blank())
#
# ggsave(here("data", "sentiment-raw.png"))

sentiment <-
  sentiment %>%
  select(title, sentiment) %>%
  nest(sentiment)

overall <- data_frame(title = "0. Whole Series", data = list(bind_rows(sentiment$data)))
sentiment <- bind_rows(sentiment, overall)

minwiggle <- 1
maxwiggle <- 10
wiggliness <- 3

sizes <- seq(minwiggle, maxwiggle)

arcs <-
  sentiment %>%
  .[rep(1:8, each = length(sizes)), ] %>%
  mutate(low_pass_size = rep(sizes, length.out = 8 * length(sizes))) %>%
  rowwise %>%
  mutate(transformed = list(get_transformed_values(data[[1]],
                                                   low_pass_size = low_pass_size,
                                                   x_reverse_len = 100,
                                                   scale_vals = TRUE,
                                                   scale_range = FALSE))) %>%
  select(-data) %>%
  unnest %>%
  group_by(title, low_pass_size) %>%
  mutate(row = row_number()) %>%
  ungroup

saveRDS(arcs, here("data", "dashboard/arcs.Rds"))
arcs <- readRDS(here("data", "dashboard/arcs.Rds"))

ranges <-
  arcs %>%
  filter(low_pass_size >= 3, low_pass_size <= 10) %>%
  group_by(title, row) %>%
  summarise(min = min(transformed), max = max(transformed))

main <-
  ggplot() +
  ylim(-3.5, 3.5) +
  facet_wrap(~title, ncol = 2, dir = "v") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  labs(title = "Sentiment in Harry Potter") +
  scale_x_discrete(expand=c(0,0)) +
  theme(plot.title = element_text(size = 20)) +
  theme(strip.text = element_text(hjust = 0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(strip.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(margin = margin(r = -10))) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank())

hline <-
  geom_hline(yintercept = 0, alpha = .3)

ribbon <-
  geom_ribbon(aes(row, ymin = min, ymax = max),
              fill = "grey85",
              data = ranges)

arcline <-
  geom_line(aes(row, transformed),
            colour = "#2780E3",
            size = 1,
            data = arcs %>% filter(low_pass_size == wiggliness))

arcarea <-
  geom_area(aes(row, transformed),
            fill = "#2780E3",
            alpha = .5,
            data = arcs %>% filter(low_pass_size == wiggliness))

# main + arcarea
# ggsave(here("data", "sentiment-arcarea.png"))

# main + ribbon + hline
# ggsave(here("data", "sentiment-ribbon.png"))

# main + ribbon + arcline + hline
# ggsave(here("data", "sentiment-ribbon-arcline.png"))

# rmarkdown::run(here("data", "dashboard", "dashboard.Rmd"))
