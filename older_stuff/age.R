
cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  filter(pew_bornagain == 1) %>% 
  filter(race == 1) %>% 
  group_by(nonden) %>% 
  ct(age, wt = commonweight) 


cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  filter(pew_bornagain == 1) %>% 
  filter(race == 1) %>% 
  filter(nonden != "NA") %>% 
  ggplot(., aes(x = nonden, y = age, color = nonden)) +
  scale_color_tableau() + 
  geom_quasirandom(size = .25) +
  theme_gg("Josefin Sans") +
  scale_y_continuous(limits = c(18, 100)) +
  add_text(x = 1, y = 98, word = "Avg Age:\n54.4", sz = 5) +
  add_text(x = 2, y = 98, word = "Avg Age:\n50.5", sz = 5) +
  labs(x = "", y = "", title = "Age Distribution of Protestant Traditions", caption = "CCES 2018") +
  ggsave("D://nd/graph_off/ageswarms_cces.png", type = "cairo-png")


cces18 %>% 
    mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                           religpew_protestant == 3 ~ "Non-Denominational")) %>% 
    mutate(age = 2018 - birthyr) %>% 
    filter(pew_bornagain == 1) %>% 
    filter(race == 1) %>% 
    filter(nonden != "NA") %>% 
    group_by(nonden) %>% 
    mean_ci(age, wt = commonweight)

## Checking our Survey - Remember the Weight ####
nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  filter(white == 1) %>% 
  group_by(nonden) %>% 
  mean_ci(age, wt = weight)


nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  filter(white == 1) %>% 
  mutate(age2 = car::recode(age, "18:35 = 1; else =0")) %>% 
  group_by(nonden) %>% 
  ct(age2, wt = weight) %>% 
  filter(age2 == 1)



cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(age2 = car::recode(age, "18:35 = 1; else =0")) %>% 
  filter(pew_bornagain == 1) %>% 
  filter(race == 1) %>% 
  filter(nonden != "NA") %>% 
  group_by(nonden) %>% 
  ct(age2, wt = commonweight) %>% 
  filter(age2 == 1)


cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(age2 = car::recode(age, "75:100=1; else =0")) %>% 
  filter(pew_bornagain == 1) %>% 
  filter(race == 1) %>% 
  group_by(nonden) %>% 
  mean_ci(age2, wt = commonweight) 
  