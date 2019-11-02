graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                    nonden=frcode(nonden==0 ~ "Denominational",
                                  nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ct(q19_1, show_na = FALSE)


dat_text <- data.frame(
  nonden = c("Denominational", "Non-Denominational"),
  label   = c("Mean = 2.96", "Mean = 3.48")
)

graph %>% 
  ggplot(., aes(x = q19_1, y = pct, fill = nonden)) +
  geom_col(color = "black") + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  geom_text(aes(y = pct + .01, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 3, family = "font") +
  labs(x = "", y = "", title = "How Many Times Have You Switched Churches?") +
  theme(plot.title = element_text(size = 14)) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = -Inf, label = label), hjust = -3.5, vjust = -12, family = "font") +
  ggsave("numberofchurches.png", type = "cairo-png") 

  

nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
              nonden=frcode(nonden==0 ~ "Denominational",
                            nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  mean_ci(q19_1)