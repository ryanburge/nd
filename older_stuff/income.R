graph <- nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  mutate(inc = frcode(q100 == 1 ~ "Less\nthan\n$25k",
                      q100 == 2 ~ "$25k\nto\n$35k",
                      q100 == 3 ~ "$35k\nto\n$50k",
                      q100 == 4 ~ "$50k\nto\n$75k",
                      q100 == 5 ~ "$75k\nto\n$100k",
                      q100 == 6 ~ "$100k\nto\n$150k",
                      q100 == 7 ~ "$150k\nto\n$200k",
                      q100 == 8 ~ "$200k\nor\nMore")) %>%
  group_by(nonden) %>% 
  ct(inc, wt = weight, show_na = FALSE)


graph %>% 
  ggplot(., aes(x = inc, y = pct, fill = nonden)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  labs(x = "", y = "", title = "Total Yearly Household Income", caption = "Non-Denom Survey") +
  # annotate("rect", xmin = 3.5, xmax = 6.6, ymin = 0, ymax = Inf, alpha = .2) +
  # geom_text(data = dat_text, mapping = aes(x = -Inf, y = -Inf, label = label), hjust = -1.85, vjust = -14, family = "font") +
  # theme(plot.title = element_text(size = 12)) +
  ggsave("D://nd/graph_off/income.png", type = "cairo-png") 