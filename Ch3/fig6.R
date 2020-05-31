
nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  group_by(nonden) %>% 
  ggplot(., aes(x = nonden, y = age, color = nonden)) +
  scale_color_tableau() + 
  geom_quasirandom(size = .25) +
  theme_gg("Josefin Sans") +
  scale_y_continuous(limits = c(18, 100)) +
  add_text(x = 1, y = 98, word = "Avg Age:\n53.1", sz = 5) +
  add_text(x = 2, y = 98, word = "Avg Age:\n52.6", sz = 5) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://nd/Ch3/images/fig6.png", type = "cairo-png", width = 7)

nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  group_by(nonden) %>%
  mean_ci(age, wt = weight)


mode <- function(codes){
  which.max(tabulate(codes))
}

nd %>% 
  mutate(bagain = q16) %>% 
  filter(bagain == 1) %>% 
  group_by(nonden) %>%
  summarise(m2 = mode(age))



nd %>% 
  filter(bagain == 1) %>% 
  group_by(nonden) %>%
  mutate(age2 = car::recode(age, "18:35=1; else =0")) %>% 
  mean_ci(age2, wt = weight) 


cces18 %>% 
  mutate(nonden = frcode(religpew_protestant != "NA" & religpew_protestant != 3 ~ "Denomninational",
                         religpew_protestant == 3 ~ "Non-Denominational")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  filter(pew_bornagain == 1) %>% 
  group_by(nonden) %>% 
  mean_ci(age, wt = commonweight) %>% 
  na.omit()