all <- read_csv("D://nd/all_geocodes.csv") 
denom <- read_csv("D://yp_scrapes/denom.csv")


tidy_col <- all %>% 
  mutate(id = name) %>% 
  unnest_tokens(word, name)

col_w_den <- tidy_col %>% 
  filter(word %in% denom$word) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(type = "Denominational Identifier")

col_wo_den <- tidy_col %>% 
  filter(!word %in% col_w_den$id) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(type = "No Denominational Identifier")

join <- bind_rows(col_w_den, col_wo_den)

gg2 <- join %>% 
  group_by(city) %>% 
  ct(type)


gg2 %>% 
  ggplot(., aes(x = 1, y = pct, fill = type)) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ city, ncol =1, strip.position = "left") +
  scale_fill_manual(values = c("steelblue1", "darkorange2")) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y.left = element_text(angle=0)) +
  guides(fill = guide_legend(reverse=T)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 4, family = "font") +
  labs(x = "", y = "", title = "", subtitle = "", caption = "") +
  ggsave("D://nd/Ch1/images/fig5.png", width = 9, height = 7)