graph <- nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  mutate(educ = frcode(q96 == 1 ~ "Less than\nHigh School",
                       q96 == 2 ~ "High\nSchool",
                       q96 == 3 ~ "Some\nCollege",
                       q96 == 4 ~ "2 Year\nCollege Grad.",
                       q96 == 5 ~ "4 Year\nCollege Grad.",
                       q96 == 6 ~ "Graduate\nEducation")) %>% 
  group_by(nonden) %>% 
  ct(educ, wt = weight)


graph %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(educ))) +
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
  labs(x = "", y = "", title = "Education Breakdown", caption = "Non-Denom Survey") +
  ggsave("D://nd/graph_off/education.png", type = "cairo-png", height = 3, width = 8)