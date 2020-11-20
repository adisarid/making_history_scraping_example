library(polite)
library(rvest)
library(tidyverse)
library(assertthat)


# function which is used to scrape an episode from making history
scrape_episode <- function(episode_num){
  
  # validation checks
  assert_that(is.number(episode_num), msg = "episode_num must be numeric")
  assert_that(between(episode_num, 1, 500), msg = "episode_num must be within 1 and 500")
  
  # generate url
  episode_url <- paste0("https://www.osimhistoria.com/osimhistoria/oh_ep_", 
                        str_pad(episode_num, width = 3, side = "left", pad = "0"))
  
  # initial scrape 
  # (this is the polite version, the blunt version is to use read_html(episode_url) instead of bow/scrape)
  raw_script <- bow(episode_url,
                    delay = 5) %>%
    scrape() %>%
    html_nodes(paste0("body ", paste0(".font_", 0:10, collapse = ", "))) %>% 
    html_attrs_dfr() %>% 
    as_tibble() %>% 
    filter(.text != "") %>% 
    mutate(paragraph = seq_along(.text)) %>% 
    rename(text = .text) %>% 
    mutate(episode = episode_num)
  
}

# example use
# ep1 <- scrape_episode(1)

# safe variation:
safe_scrape_episode <- possibly(scrape_episode, 
                                otherwise = NA, 
                                quiet = TRUE)

# scrape multiple episodes
pb <- progress::progress_bar$new(total = 333, format = "[:bar] :percent eta: :eta")

scraped_episodes <- tibble(episode_num = 1:333) %>% 
  mutate(episode_script = map(episode_num, ~{
    pb$tick()
    safe_scrape_episode(.)
    }))

# saveRDS(scraped_episodes, file = "ScrapedMakingHistory.rds")