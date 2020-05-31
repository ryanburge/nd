regg <- nd %>% 
  mutate(sign = car::recode(q47_1, "2=1; 1=0")) %>% 
  mutate(button = car::recode(q47_2, "2=1; 1=0")) %>% 
  mutate(townhall = car::recode(q47_3, "2=1; 1=0")) %>%
  mutate(protest = car::recode(q47_4, "2=1; 1=0")) %>% 
  mutate(money = car::recode(q47_5, "2=1; 1=0")) %>% 
  mutate(contact = car::recode(q47_6, "2=1; 1=0")) %>% 
  mutate(attend = q17) %>% 
  mutate(polact = sign + button + townhall + protest + money + contact)  %>% 
  mutate(nonden=car::recode(q3, "1=1; 2:17=0")) %>% 
  mutate(nonden=frcode(nonden==0 ~ "Denominational", 
                       nonden==1 ~ "Non-Denominational")) %>% 
  mutate(educ=q96, samecomm=q99, income=q100, pid7 = q27)  %>% 
  mutate(ba = frcode(q16 == 1  ~ "Born-Again",
                     q16 == 2 ~ "Not Born-Again"))
  

reg1 <- lm(polact ~ attend*nonden*ba + educ + income + pid7, data = regg)

gg2 <- interact_plot(reg1, pred = attend, modx = nonden, mod2 = ba, interval = TRUE, int.width = .76, mod2.labels = c("Born-Again", "Not Born-Again"))

gg2 + 
  scale_x_continuous(labels = c("Less than\nOnce a Month", "Once\na Month", "2-3x\na Month", "Once\na Week", "2-3x\na Week", "Daily")) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  labs(x = "Church Attendance", y = "Number of Political Activities", title = "Impact of Attendance on Political Activity") +
  ggsave("D://nd/polact_interact_ba.png", width = 12) 