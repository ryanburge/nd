
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

prri <- read_csv("D://metro_nones.csv")

scatter <- left_join(pct, prri) %>% 
  mutate(nones = nones/100)

scatter %>% 
  ggplot(., aes(x = pct, y = nones)) +
  geom_point() +
  geom_smooth(method = lm, linetype = "twodash") +
  theme_gg("Josefin Sans") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  geom_text_repel(data = scatter, aes(x = pct,y = nones, label=city), size = 4.5, family = "font") +
  theme(plot.title = element_text(size = 14)) +
  labs(x = "Percent with 'Church' in Name", y = "Percent Unaffiliated in the Metro Area", title = "", caption = "") +
  ggsave("D://nd/Ch1/images/fig4.png", type = "cairo-png", width = 8)