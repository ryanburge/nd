nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>%  
  ggplot(., aes(x = q56_1, y = nonden)) + 
  geom_density_ridges(aes(fill = nonden), scale =3, size = .03, alpha = .5) +
  scale_fill_tableau() +
  theme_gg("Josefin Sans") +
  labs(x = "Number of Church Activities", y = "", title = "Congregational Activities", caption = "Non-Denom Survey") +
  ggsave("D://nd/graph_off/activities.png", type = "cairo-png")


nd %>% 
  group_by(nonden) %>% 
  ct(q56_1) %>% as.data.frame()


nd %>% 
  group_by(nonden) %>% 
  mean_ci(q56_1) %>% as.data.frame()