nd <- nd %>% 
  mutate(inc1 = car::recode(q30_1, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(inc2 = car::recode(q30_2, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(inclusive = inc1 + inc2 + q30_3 + q30_4) %>% 
  mutate(inclusive = inclusive - 7) %>% 
  mutate(inclusive = inclusive/13)


graph <- nd %>% 
  mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
             nonden=frcode(nonden==0 ~ "Denominational",
                           nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ct(inclusive, show_na = FALSE)


graph %>% 
  ggplot(., aes(x = inclusive, y = pct, fill = inclusive)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  y_pct() + 
  geom_text(aes(y = pct + .01, label = paste0(pct*100, '%')), size = 4, family = "font") +
  labs(x = "<-- More Exclusve : More Inclusive --->", y = "", title = "Inclusivity Scale") +
  theme(plot.title = element_text(size = 12)) +
  ggsave("inclusive_scale.png", type = "cairo-png", width = 7) 