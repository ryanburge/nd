
names <- read_csv("D://yp_scrapes/full_names.csv")

names <- names %>% 
  distinct(id, .keep_all = TRUE)

total <- names %>% 
  ct(city) %>% 
  select(city, n)

church <- tidy %>% 
  group_by(city) %>% 
  ct(word) %>% 
  filter(word == "church") %>% 
  select(city, church_n = n)

pct <- left_join(total, church) %>% 
  mutate(pct = church_n/n)

pct %>% 
  ggplot(., aes(x = reorder(city, pct), y = pct)) +
  geom_col() +
  theme_gg("Josefin Sans") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://nd/Ch1/images/fig3.png", type = "cairo-png")