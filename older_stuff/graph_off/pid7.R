graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                       nonden=frcode(nonden==0 ~ "Denominational",
                                     nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ct(q27, show_na = FALSE) %>% 
  mutate(pid7 = frcode(q27 == 1 ~ "Strong\nDemocrat",
                       q27 == 2 ~ "Democrat",
                       q27 == 3 ~ "Ind.,\nLean Dem.",
                       q27 == 4 ~ "Independent",
                       q27 == 5 ~ "Ind.,\nLean Rep.",
                       q27 == 6 ~ "Republican",
                       q27 == 7 ~ "Strong\nRepublican"))


graph %>% 
  ggplot(., aes(x = pid7, y = pct, fill = pid7)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  y_pct() + 
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  labs(x = "", y = "", title = "Which of these party labels bests describes you?") +
  theme(plot.title = element_text(size = 12)) +
  ggsave("pid7.png", type = "cairo-png", width = 7) 

graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                       nonden=frcode(nonden==0 ~ "Denominational",
                                     nonden==1 ~ "Non-Denominational")) %>% 
  filter(q16 ==1) %>% 
  group_by(nonden) %>% 
  ct(q27, show_na = FALSE) %>% 
  mutate(pid7 = frcode(q27 == 1 ~ "Strong\nDemocrat",
                       q27 == 2 ~ "Democrat",
                       q27 == 3 ~ "Ind.,\nLean Dem.",
                       q27 == 4 ~ "Independent",
                       q27 == 5 ~ "Ind.,\nLean Rep.",
                       q27 == 6 ~ "Republican",
                       q27 == 7 ~ "Strong\nRepublican"))


graph %>% 
  ggplot(., aes(x = pid7, y = pct, fill = pid7)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  y_pct() + 
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  labs(x = "", y = "", title = "Which of these party labels bests describes you?", subtitle = "Only Born-Again") +
  theme(plot.title = element_text(size = 12)) +
  ggsave("pid7_ba.png", type = "cairo-png", width = 7) 