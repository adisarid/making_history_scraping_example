library(tidyverse)

osim_raw <- readRDS("data/ScrapedMakingHistory.rds")


# Rearrange to flat tibble ------------------------------------------------

osim <- osim_raw %>% 
  unnest(episode_script) %>% 
  group_by(episode_num) %>% 
  mutate(sponsership_row = str_detect(text, "חסויות")) %>% 
  mutate(sponsership_row = cumsum(sponsership_row)) %>% 
  filter(sponsership_row >= 1) %>% 
  filter(!str_detect(text, "חסויות")) %>% 
  select(-sponsership_row) %>% 
  mutate(paragraph = paragraph - min(paragraph) + 1) %>% 
  ungroup() %>% 
  select(-episode) %>% 
  filter(dir == "RTL") %>% 
  select(-dir) %>% 
  select(-style)
           

osim %>% 
  count(class) %>% 
  View()
  

# Just curious, number of words per episode -------------------------------

words_per_episode <- osim %>% 
  select(-class) %>% 
  tidytext::unnest_tokens(output = "word", input = "text")
  

words_per_episode_clean <- words_per_episode %>% 
  count(episode_num) %>% 
  filter(n > 500) %>% 
  filter(n <= 8000)

words_per_episode_clean %>% 
  filter(n > 500) %>% 
  filter(n <= 8000) %>% 
  ggplot(aes(x = episode_num, y = n)) +
  geom_point() + 
  stat_smooth(method = "lm")
  
lm(n ~ episode_num, words_per_episode_clean) %>% summary()



# Clean up stopwords ------------------------------------------------------

`%nin%` <- function(x,y){!(x %in% y)}

hebstopwords <- read_table("https://raw.githubusercontent.com/gidim/HebrewStopWords/master/heb_stopwords.txt", col_names = "stopword")

words_no_stop <- words_per_episode %>% 
  filter(word %nin% hebstopwords$stopword)

word_count <- words_no_stop %>% 
  filter(word %nin% c("שבו",
                      "בשם",
                      "כפי",
                      "פי",
                      "בלבד",
                      "בלתי",
                      "רן",
                      "ידי",
                      "אינו",
                      "לכך",
                      "כלל",
                      "עבור",
                      "שתי",
                      "באותו",
                      "רב")) %>% 
  count(word, sort = T)
  

grams2 <- osim %>% 
  select(-class) %>% 
  tidytext::unnest_ngrams(output = "word", input = "text", n = 2) %>% 
  count(word, sort = T)
