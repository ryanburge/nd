graph <- nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  mutate(marry = frcode(q98 == 1 ~ "Married",
                       q98 == 2 ~ "Single",
                       q98 == 3 ~ "Divorced",
                       q98 == 4 ~ "Widowed")) %>% 
  group_by(nonden) %>% 
  ct(marry, wt = weight)


graph %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(marry))) +
  geom_col(color = "black") +
  coord_flip() +
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  scale_fill_lancet() +
  y_pct() +
  guides(fill = guide_legend(reverse=T, nrow =1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.04, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "white") +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  theme(strip.text.y = element_text(angle = 180)) +
  labs(x = "", y = "", title = "Marital Status", caption = "Non-Denom Survey") +
  ggsave("D://nd/graph_off/married.png", type = "cairo-png", height = 3, width = 8)