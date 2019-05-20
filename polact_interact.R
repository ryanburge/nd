regg <- nd %>% 
  mutate(sign = car::recode(Q47_1, "2=1; 1=0")) %>% 
  mutate(button = car::recode(Q47_2, "2=1; 1=0")) %>% 
  mutate(townhall = car::recode(Q47_3, "2=1; 1=0")) %>%
  mutate(protest = car::recode(Q47_4, "2=1; 1=0")) %>% 
  mutate(money = car::recode(Q47_5, "2=1; 1=0")) %>% 
  mutate(contact = car::recode(Q47_6, "2=1; 1=0")) %>% 
  mutate(polact = sign + button + townhall + protest + money + contact) 

reg1 <- lm(polact ~ attend*nonden + educ + income + pid7, data = regg)

gg2 <- interact_plot(reg1, pred = attend, modx = nonden, interval = TRUE, int.width = .76)

gg2 + 
  ggsave("D://nd/polact_interact.png")