nd <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                    nonden=frcode(nonden==0 ~ "Denominational",
                                  nonden==1 ~ "Non-Denominational")) 


graph <- nd %>% 
  mutate(ba = frcode(q16 == 1  ~ "Born-Again",
                     q16 == 2 ~ "Not Born-Again")) %>% 
  group_by(nonden) %>% 
  ct(ba, wt = weight)


graph %>% 
  ggplot(., aes(x=1, y = pct, fill =fct_rev(ba))) +
  geom_col(color = "black") +
  coord_flip() +
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  scale_fill_npg() +
  y_pct() +
  guides(fill = guide_legend(reverse=T, nrow =1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 4, family = "font") +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  theme(strip.text.y = element_text(angle = 180)) +
  labs(x = "", y = "", title = "Would you describe yourself as a born-again or evangelical Christian, or not?", caption = "") +
  ggsave("D://nd/graph_off/ba_status.png", type = "cairo-png", height = 3, width = 8)
