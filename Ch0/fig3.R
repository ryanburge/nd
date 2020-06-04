

first <- clean %>% 
  group_by(prot10) %>% 
  ct(prot12, wt = weight, show_na = FALSE) %>% 
  ungroup(prot10)

first1 <- first %>%
  mutate(prot10 = as.character(prot10)) %>% 
  mutate(prot12 = as.character(prot12)) %>% 
  filter(prot10 == prot12) %>% 
  mutate(years = "2010-2012") %>% 
  rename(prot1 = prot10) %>% 
  rename(prot2 = prot12)



second <- clean %>% 
  group_by(prot12) %>% 
  ct(prot14, wt = weight, show_na = FALSE) %>% 
  ungroup(prot12)

second1 <- second %>%
  mutate(prot12 = as.character(prot12)) %>% 
  mutate(prot14 = as.character(prot14)) %>% 
  filter(prot12 == prot14) %>% 
  mutate(years = "2012-2014") %>% 
  rename(prot1 = prot12) %>% 
  rename(prot2 = prot14)



third <- clean %>% 
  group_by(prot10) %>% 
  ct(prot14, wt = weight, show_na = FALSE) %>% 
  ungroup(prot10)

third1 <- third %>%
  mutate(prot10 = as.character(prot10)) %>% 
  mutate(prot14 = as.character(prot14)) %>% 
  filter(prot10 == prot14) %>% 
  mutate(years = "2010-2014")%>% 
  rename(prot1 = prot10) %>% 
  rename(prot2 = prot14)


graph <- bind_rows(first1, second1, third1)

graph$facet <- factor(graph$years, levels=c('2010-2012','2012-2014','2010-2014'))


graph %>% 
  ggplot(., aes(x = prot1, y = pct)) +
  geom_col(color = "black") +
  theme_gg("Josefin Sans") +
  facet_wrap(~ facet) +
  coord_flip() +
  lab_bar(top = FALSE, type = pct, pos = .15, sz = 4.5) +
  scale_y_continuous(labels = percent) + 
  labs(x = "", y = "", title = "Retention Rate Among Denominations", caption = "") +
  ggsave("D://nd/Ch0/images/fig3.png", type = "cairo-png", width = 8)