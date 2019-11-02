nd <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                      nonden=frcode(nonden==0 ~ "Denominational",
                                    nonden==1 ~ "Non-Denominational")) %>% 
  mutate(attend = q17) 

graph <- nd %>% 
  group_by(nonden) %>% 
  ct(attend, wt = weight) %>% 
  mutate(attend = frcode(attend == 2 ~ "Less than\nOnce a\nMonth", 
                         attend == 3 ~ "Once\na Month", 
                         attend == 4 ~ "2-3x\na Month",
                         attend == 5 ~ "Once\na Week",
                         attend == 6 ~ "2-3x\na Week",
                         attend == 7 ~ "Daily"))


graph %>% 
  ggplot(., aes(x = attend, y = pct, fill = nonden)) +
  geom_col(color = "black", position = "dodge") +
  y_pct() +
  theme_gg("Josefin Sans") +
  geom_text(aes(y = pct + .01, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 3, family = "font") +
  ggthemes::scale_fill_tableau() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", title = "Church Attendance by Denominational Status") +
  ggsave("attend_dodged_bars.png", width = 6)
  

cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protes %>% tant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  filter(pew_bornagain == 1) %>%  
  mutate(att = car::recode(pew_churatd, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att = frcode(att == 1 ~ "Never",
                      att == 2 ~ "Seldom",
                      att == 3 ~ "Yearly",
                      att == 4 ~ "Monthly",
                      att == 5 ~ "Weekly",
                      att == 6 ~ "Weekly+")) %>% 
  group_by(nonden) %>% 
  ct(att, wt = commonweight, show_na = FALSE) %>% na.omit()


  

