graph <- cces18 %>% 
  filter(pew_bornagain == 1) %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>%
  mutate(size = frcode(urbancity == 4 ~ "Rural Area",
                       urbancity == 3 ~ "Town",
                       urbancity == 2 ~ "Suburb",
                       urbancity == 1 ~ "City")) %>% 
  group_by(nonden) %>% 
  ct(size, wt = commonweight, show_na = FALSE) %>% 
  na.omit()


graph %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(size))) +
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
  labs(x = "", y = "", title = "Geography of Protestant Churches", caption = "CCES 2018") +
  ggsave("D://nd/graph_off/geography_cces.png", type = "cairo-png", height = 3, width = 8)


graph <- nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  mutate(size = frcode(q67 == 1 ~ "Rural",
                       q67 == 2 | q67 == 3 | q67 == 4  ~ "Town",
                       q67 == 6 | q67 == 8 ~ "Suburb",
                       q67 == 5 | q67 == 7 ~ "City")) %>% 
  group_by(nonden) %>% 
  ct(size, wt = weight)


graph %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(size))) +
  geom_col(alpha = .7) +
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
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://nd/graph_off/geography_ours.png", type = "cairo-png", height = 3, width = 8)