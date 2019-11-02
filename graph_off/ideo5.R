graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                       nonden=frcode(nonden==0 ~ "Denominational",
                                     nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ct(q28, show_na = FALSE) %>% 
  mutate(ideo5 = frcode(q28 == 5 ~ "Strong\nLiberal",
                       q28 == 4 ~ "Liberal",
                       q28 == 3 ~ "Moderate",
                       q28 == 2 ~ "Conservative",
                       q28 == 1 ~ "Strong\nConservative"))


graph %>% 
  ggplot(., aes(x = ideo5, y = pct, fill = ideo5)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_manual(values = c("#000080", "#0080FF", "azure3", "#EA3C53", "#8D021F")) +
  y_pct() + 
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  labs(x = "", y = "", title = "Which of these labels bests describes you?") +
  theme(plot.title = element_text(size = 12)) +
  ggsave("ideo5.png", type = "cairo-png", width = 7) 


graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                       nonden=frcode(nonden==0 ~ "Denominational",
                                     nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  filter(q16 ==1) %>% 
  ct(q28, show_na = FALSE) %>% 
  mutate(ideo5 = frcode(q28 == 5 ~ "Strong\nLiberal",
                        q28 == 4 ~ "Liberal",
                        q28 == 3 ~ "Moderate",
                        q28 == 2 ~ "Conservative",
                        q28 == 1 ~ "Strong\nConservative"))


graph %>% 
  ggplot(., aes(x = ideo5, y = pct, fill = ideo5)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_manual(values = c("#000080", "#0080FF", "azure3", "#EA3C53", "#8D021F")) +
  y_pct() + 
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  labs(x = "", y = "", title = "Which of these labels bests describes you?", subtitle = "Only Born-Again") +
  theme(plot.title = element_text(size = 12)) +
  ggsave("ideo5_ba.png", type = "cairo-png", width = 7) 