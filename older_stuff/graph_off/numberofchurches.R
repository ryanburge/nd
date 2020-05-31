
graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                    nonden=frcode(nonden==0 ~ "Denominational",
                                  nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  mutate(switching=case_when(q18 == 1 | q19_1 == 0 ~ 0,
                             q19_1==1 ~ 1,
                             q19_1==2 ~ 2,
                             q19_1==3 ~ 3,
                             q19_1==4 ~ 4,
                             q19_1==5 ~ 5,
                             q19_1==6 ~ 6,
                             q19_1==7 ~ 7,
                             q19_1==8 ~ 8,
                             q19_1==9 ~ 9,
                             q19_1==10 ~ 10)) %>% 
  group_by(nonden) %>% 
  ct(switching, show_na = FALSE, wt = weight)

nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
              nonden=frcode(nonden==0 ~ "Denominational",
                            nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  mutate(switching=case_when(q18 == 1 | q19_1 == 0 ~ 0,
                             q19_1==1 ~ 1,
                             q19_1==2 ~ 2,
                             q19_1==3 ~ 3,
                             q19_1==4 ~ 4,
                             q19_1==5 ~ 5,
                             q19_1==6 ~ 6,
                             q19_1==7 ~ 7,
                             q19_1==8 ~ 8,
                             q19_1==9 ~ 9,
                             q19_1==10 ~ 10)) %>% 
  group_by(nonden) %>% 
  mean_ci(switching, wt = weight)


dat_text <- data.frame(
  nonden = c("Denominational", "Non-Denominational"),
  label   = c("Mean = 1.85", "Mean = 2.91")
)

graph %>% 
  ggplot(., aes(x = switching, y = pct, fill = nonden)) +
  geom_col(alpha = .7) + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  geom_text(aes(y = pct + .0125, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 3, family = "font") +
  labs(x = "", y = "", title = "") +
  theme(plot.title = element_text(size = 14)) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = -Inf, label = label), hjust = -3.5, vjust = -12, family = "font") +
  ggsave("D://nd/switching.png", type = "cairo-png") 

  