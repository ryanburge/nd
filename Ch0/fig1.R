
graph1 <- gss %>% 
  mutate(nd = case_when(denom == 70 ~ 1, TRUE ~ 0)) %>% 
  group_by(year) %>% 
  mean_ci(nd) %>% 
  mutate(type = "Non-Denom")

graph2 <- gss %>% 
  group_by(year) %>% 
  mean_ci(nofaith) %>% 
  mutate(type = "Nones")

graph3 <- gss %>% 
  mutate(nde = case_when(evangelical == 1 & denom != 70 ~ 1, TRUE ~ 0)) %>% 
  group_by(year) %>% 
  mean_ci(nde) %>% 
  mutate(type = "Denom. Evangelicals")

gg <- bind_rows(graph1, graph2, graph3)



gg %>% 
  ggplot(., aes(x = year, y = mean, group = type, color = type)) +
  geom_point(size=3, color="white") +
  geom_point(size=2, shape=1) +
  geom_point(size=1, shape=19) +
  geom_smooth(se = FALSE, linetype = "twodash") +
  theme_gg("Josefin Sans") +
  scale_y_continuous(labels = percent) +
  scale_color_jco() +
  add_text(x = 2007, y = .22, word = "Denom. Evangelicals", sz = 4) +
  add_text(x = 2000, y = .15, word = "Nones", sz = 4) +
  add_text(x = 2010, y = .08, word = "Non-Denom.", sz = 4) +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://nd/Ch0/images/fig1.png", type = "cairo-png", width = 8)