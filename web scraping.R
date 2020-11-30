library(tidytext)
library(ggpubr) 
library(tidyverse)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)

url <- c("http://transcripts.cnn.com/TRANSCRIPTS/2009/21/sn.01.html", "http://transcripts.cnn.com/TRANSCRIPTS/2009/23/cnr.02.html")

cong_wbpg <- read_html(url[2])
cong_wbpg

cong_wbpg %>%
  html_nodes("pre") %>%
  html_text()

cong_wbpg %>%
  html_node("title") %>%
  html_text()

hearings <- c()
contents <- c()
for(i in 1:length(url)){ 
  
  cong_wbpg <- read_html(url[i])
  body <- cong_wbpg %>%
    html_nodes("pre") %>%
    html_text()
  contents = append(contents, body)
  
  cong_wbpg <- read_html(url[i])
  hearing <- cong_wbpg %>%
    html_node("title") %>%
    html_text()  
  hearings = append(hearings, rep(hearing,each=length(body)))
  
}

hearing_text <- data.frame(hearing = hearings, text = contents) 

hearing_words <- hearing_text %>%
  unnest_tokens(word, text) %>%
  count(hearing, word, sort = TRUE)

total_words <- hearing_words %>% 
  group_by(hearing) %>% 
  summarize(total = sum(n))

hearing_words_data <- left_join(hearing_words, total_words)
hearing_words_data

ggplot(hearing_words_data, aes(n/total, fill = hearing)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~hearing, ncol = 2, scales = "free_y")

freq_by_rank <- hearing_words_data %>% 
  group_by(hearing) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = hearing)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

hearing_words_data <- hearing_words_data %>%
  bind_tf_idf(word, hearing, n)
hearing_words_data

hearing_words_data %>%
  select(-total) %>%
  arrange(desc(tf_idf))

hearing_words_data %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(hearing) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = hearing)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hearing, ncol = 2, scales = "free") +
  coord_flip()


