regg <- nd %>% 
  mutate(switch = q19_1) %>% 
  mutate(moved16 = car::recode(q99, "1=1; 2=0; else = NA")) %>% 
  mutate(movedfall = car::recode(q93, "3:4=1; 1:2=0; else = NA")) %>% 
  mutate(educ = q96) %>% 
  mutate(age = 2019- q90_1) %>%
  mutate(male = car::recode(q89, "1=1; else =0")) %>% 
  mutate(white = case_when(q95_1 == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(literal = car::recode(q58, "1=1; else =0")) %>% 
  mutate(nonden=car::recode(q3, "1=1; 2:17=0"))  %>% 
  mutate(keepout = car::recode(q50_1, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(endorse = car::recode(q50_2, "5=5; 4=4; 3=3; 2=2; 1=1; else = NA")) %>% 
  mutate(register = car::recode(q50_3, "1=1; 2=2; 3=3; 4=4; 5=5; else = NA")) %>% 
  mutate(forums = car::recode(q50_4, "1=1; 2=2; 3=3; 4=4; 5=5; else = NA")) %>%
  mutate(good = car::recode(q50_5, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(uncomfort = keepout + endorse + register + forums + good ) %>% 
  mutate(uncomfort = uncomfort/25) %>% 
  mutate(types = as.factor(q67)) %>% 
  mutate(pid7 = as.factor(q27))
  

reg1 <- lm(switch ~ moved16 + movedfall + educ + age + male + white + literal + nonden + uncomfort + relevel(pid7, ref = "4"), data = regg)