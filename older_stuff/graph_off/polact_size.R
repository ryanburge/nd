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
                     q16 == 2 ~ "Not Born-Again")) %>% 
  mutate(size = car::recode(q24, "4=1; 7=2; 8=3; 9=4; 10=5; 11=6")) %>% 
  mutate(nonden=car::recode(q3, "1=1; 2:17=0"))


reg1 <- lm(polact ~ I(size^2)*nonden + educ + income + pid7, data = regg)

gg2 <- interact_plot(reg1, pred = size, modx = nonden, interval = TRUE, int.width = .76)


gg2 + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("0-50", "51-100", "101-300", "301-500", "501-1000", "1000+")) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  labs(x = "Size of Church", y = "Number of Political Activities", title = "Impact of Church Size on Political Activity") +
  ggsave("D://nd/graph_off/polact_interact_size.png", width = 8) 


reg1 <- lm(polact ~ I(size^2) + educ + income + pid7 + nonden, data = regg)
