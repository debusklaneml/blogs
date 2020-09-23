# JRE Podcast Analysis 
library(tidyverse)
library(janitor)
library(readxl)
library(tidylog)
library(youtubecaption)
library(tidytext)

test <- get_caption('https://www.youtube.com/watch?v=hcPUoxTvw5g')
glimpse(test)

data("stop_words")

playlist <- read_csv('YouTube-Playlist.csv', 
                     skip = 1,
                     col_names = TRUE) %>% 
  clean_names() %>%
  select(-x6:-x11) %>%
  mutate(
    ep_num = str_replace_all(title, ".*#F*(\\d{4}).*", "\\1"),
    vid = str_replace_all(video_url, ".*=(.*)$", "\\1")
  ) %>% 
  filter(!is.na(channel)) %>% 
  separate(title, c('first', 'participants'), sep = '-') %>%
  select(-first, -description)

# Pull Transcripts 
safe_cap <- safely(get_caption)

jre_trans <- map(playlist$video_url, safe_cap)
glimpse(jre_trans)

res <- map(1:length(jre_trans),
           ~pluck(jre_trans, ., "result")) %>%
  compact() %>% 
  bind_rows() %>%
  inner_join(x = playlist,
             y = .,
             by = "vid") %>%
  unnest_tokens(
    input = 'text',
    output = 'word'
  ) 

res2 <- res %>%
  anti_join(stop_words) %>%
  filter(!word %in% c('yeah', 'uh', 'um', 'lot', 'gonna', '__', 'guy', 'guys', 'stuff'))

word_count <- res2 %>%
  count(word, sort = TRUE)
word_count
