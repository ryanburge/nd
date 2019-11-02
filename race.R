graph <- cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  filter(pew_bornagain == 1) %>% 
  mutate(race = frcode(race == 1 ~ "White",
                       race == 2 ~ "Black",
                       race == 3 ~ "Hispanic",
                       race == 4 ~ "Asian",
                       TRUE ~ "All Others")) %>% 
  group_by(nonden) %>% 
  ct(race, wt = commonweight) %>% 
  na.omit() %>% 
  mutate(survey = "CCES")
  

graph1 <- nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>%  
  mutate(race = frcode(q95_1 == 1 ~ "White",
                       q95_3 == 1 ~ "Black",
                       q95_2 == 1 ~ "Hispanic",
                       q95_4 == 1 ~ "Asian",
                       q95_5 == 1 ~ "All Others")) %>% 
  group_by(nonden) %>% 
  ct(race, wt = weight) %>% 
  na.omit() %>% 
  mutate(survey = "Ours")



graph %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(race))) +
  geom_col(color = "black") +
  coord_flip() +
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  scale_fill_d3() +
  y_pct() +
  guides(fill = guide_legend(reverse=T, nrow =1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.04, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "white") +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  theme(strip.text.y = element_text(angle = 180)) +
  labs(x = "", y = "", title = "Racial Breakdown", caption = "CCES 2018") +
  ggsave("D://nd/graph_off/race.png", type = "cairo-png", height = 3, width = 8)


graph1 %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(race))) +
  geom_col(color = "black") +
  coord_flip() +
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  scale_fill_d3() +
  y_pct() +
  guides(fill = guide_legend(reverse=T, nrow =1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.04, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "white") +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  theme(strip.text.y = element_text(angle = 180)) +
  labs(x = "", y = "", title = "Racial Breakdown", caption = "Non-Denom Survey") +
  ggsave("D://nd/graph_off/race_ours.png", type = "cairo-png", height = 3, width = 8)
