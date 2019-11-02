graph <- nd %>% 
  group_by(nonden) %>%
  mutate(q18 = frcode(q18 == 1 ~ "Attending\nSame Church",
                      q18 == 2 ~ "Same Denom\nDiff. Church",
                      q18 == 3 ~ "Switched to\nSomething Else",
                      q18 == 4 ~ "Not attending\n at 16")) %>% 
  ct(q18)


graph %>% 
  ggplot(., aes(x = q18, y = pct, group = nonden, fill = nonden)) +
  geom_col(color = "black", position = "dodge") +
  theme_gg("Josefin Sans") +
  scale_fill_manual(values = c('Non-Denominational' = "#FF7E0D", 'Denominational' = "#49B7FC")) +
  y_pct() + 
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 6, family = "font") +
  labs(x = "", y = "", title = "Are You Attending the Same Church Now as You Were at 16?") +
  ggsave("D://nd/graph_off/denom16.png", type = "cairo-png", width = 10, height = 8)
