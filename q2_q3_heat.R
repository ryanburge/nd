library(socsci)
library(car)
source("D://theme.R")

nd <- read_csv("D://nd/data.csv") 

nd <- nd %>% 
  mutate(nondenom = case_when(Q2 == 1 ~ 1, 
                              Q3 == 1 ~ 1, 
                              TRUE ~ 0))


nd %>% 
  filter(Q18 != "NA") %>% 
  ct(Q18)

nd <- nd %>% 
  mutate(q2 = car::recode(Q2, "1 = 'Non-Denom'; 16 = 'Protestant'; 18 = 'Other Christian'; else = NA")) %>% 
  mutate(q3 = car::recode(Q3, "1 = 'Non-Denom'; 
                          2 = 'Mennonite'; 
                          3 = 'Unitarian';
                          4 = 'Methodist'; 
                          6 = 'Lutheran';
                          7 = 'Presbyterian';
                          8 = 'Pentecostal';
                          9 = 'Episcopalian';
                          10 = 'Church of Christ'; 
                          11 = 'United C of C'; 
                          12 = 'Holiness'; 
                          13 = 'Reformed'; 
                          14 = 'Adventist'; 
                          15 = 'Jehovahs Witness';
                          16 = 'None of the Above'; 
                          17 = 'Baptist'")) 

tile <- nd %>% 
  filter(q2 != "NA") %>% 
  filter(q3 != "NA") %>% 
  group_by(q2) %>% 
  ct(q3)

tile %>% 
  ggplot(., aes(x= q2, y = q3)) +
  geom_tile(aes(fill = pct), color = "black") +
  scale_fill_gradient(low = "azure3", high = "#E94057") +
  theme_gg("Abel") +
  geom_text(aes(x= q2, y = q3, label = paste0(pct*100, '%')), size = 4, family = "font") +
  labs(x= "", y = "", title = "", subtitle = "", caption = "") +
  ggsave("D://nd/q2_q3_heat.png", width = 6)
