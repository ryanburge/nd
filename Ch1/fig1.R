library(socsci)
library(tidytext)
library(ggrepel)

names <- read_csv("D://yp_scrapes/full_names.csv")

names <- names %>% 
  distinct(id, .keep_all = TRUE)

tidy <- names %>% 
  unnest_tokens(word, name) %>% 
  anti_join(stop_words, by = c("word" = "word"))


count <- tidy %>% 
  ct(word) %>% 
  mutate(pct = n/1078) %>% 
  arrange(-pct)

count %>%
  filter(word != "church") %>% 
  top_n(25, pct) %>% 
  ggplot(., aes(x = reorder(word, pct), y = pct)) +
  geom_col(alpha = .7) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  theme_gg("Josefin Sans") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.1, color="black")) +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://nd/Ch1/images/fig1.png")