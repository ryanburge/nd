library(socsci)
library(car)
library(haven)
library(janitor)
source("D://theme.R")

nd <- read_csv("D://nd/data.csv") %>% 
  mutate(id = ResponseId)

weight <- read_dta("D://nd/rake_weights.dta") %>% 
  rename(id = responseid)

nd <- left_join(nd, weight, by="id")

nd <- nd %>% mutate(nonden=car::recode(Q3, "1=1; 2:17=0"),
                        nonden=frcode(nonden==0 ~ "Denominational",
                                      nonden==1 ~ "Non-Denominational"))

nd <- nd %>% 
  rename(baptist = Q4) %>% 
  rename(methodist = Q5) %>% 
  rename(nd = Q6) %>% 
  rename(lutheran = Q7) %>% 
  rename(presby = Q8) %>% 
  rename(pente = Q9) %>% 
  rename(epis = Q10) %>% 
  rename(christian = Q11) %>% 
  rename(cong = Q12) %>% 
  rename(holy = Q13) %>% 
  rename(reformed = Q14) %>% 
  rename(advent = Q15) %>% 
  rename(bagain = Q16) %>% 
  rename(attend = Q17) %>% 
  rename(congsize = Q24) %>% 
  rename(pid7 = Q27) %>% 
  rename(ideo5 = Q28) %>% 
  rename(demprim = Q44) %>% 
  rename(repprim = Q45) %>% 
  rename(imm_therm = Q54_1) %>% 
  rename(muslim_therm = Q54_2) %>% 
  rename(likeme_therm = Q54_3) %>% 
  rename(gop_therm = Q54_4) %>% 
  rename(dem_therm = Q54_5) %>% 
  rename(djt_therm = Q54_6) %>% 
  rename(hrc_therm = Q54_7) %>% 
  rename(gay_therm = Q54_8) %>% 
  rename(xtnfun_therm = Q54_9) %>% 
  rename(atheist_therm = Q54_10) %>% 
  rename(racist_therm = Q54_11) %>% 
  rename(bible = Q58) %>% 
  rename(size = Q67) %>% 
  rename(relimp = Q72) %>% 
  rename(gender = Q89) %>% 
  rename(birthyr = Q90_1) %>% 
  rename(state = Q94) %>% 
  rename(white = Q95_1) %>% 
  mutate(white = replace_na(white, 0)) %>% 
  rename(hispanic = Q95_2) %>% 
  mutate(hispanic = replace_na(hispanic, 0)) %>% 
  rename(black = Q95_3) %>% 
  mutate(black = replace_na(black, 0)) %>% 
  rename(asian = Q95_4) %>% 
  mutate(asian = replace_na(asian, 0)) %>% 
  rename(other = Q95_5) %>% 
  mutate(other = replace_na(other, 0)) %>% 
  rename(educ = Q96) %>% 
  rename(gay = Q97) %>% 
  rename(marital = Q98) %>% 
  rename(moved = Q99) %>% 
  rename(income = Q100)

nd <- nd %>% 
  filter(Q77 != "NA") %>% 
  mutate(sabbath = car::recode(Q77, "4=1; 5:6=0; else = NA")) 

nd <- nd %>% 
  filter(Q78 != "NA") %>% 
  mutate(disciples = car::recode(Q78, "5=1; 4=0; 6=0; else = NA")) 

nd <- nd %>% 
  filter(Q79 != "NA") %>% 
  mutate(ramadan = car::recode(Q79, "3=1; 1:2=0; else = NA")) 

nd <- nd %>% 
  filter(Q80 != "NA") %>% 
  mutate(moses = car::recode(Q80, "6=1; 4:5=0; else = NA")) 

nd <- nd %>% 
  filter(Q81 != "NA") %>% 
  mutate(gfriday = car::recode(Q81, "5=1; 4=0; 6=0; else = NA")) 

nd <- nd %>% 
  filter(Q82 != "NA") %>% 
  mutate(smith = car::recode(Q82, "6=1; 4:5=0; else = NA")) 

nd <-nd %>% 
  mutate(relknow = sabbath + disciples + ramadan + moses + gfriday + smith)

## POL KNOW ####
nd <- nd %>% 
  filter(Q84 != "NA") %>% 
  mutate(pence = car::recode(Q84, "6=1; 5=0; 7=0; else = NA")) 

nd <- nd %>% 
  filter(Q85 != "NA") %>% 
  mutate(pelosi = car::recode(Q85, "6=1; 4:5=0; else = NA")) 

nd <- nd %>% 
  filter(Q86 != "NA") %>% 
  mutate(merkel = car::recode(Q86, "4=1; 5:6=0; else = NA")) 

nd <- nd %>% 
  filter(Q87 != "NA") %>% 
  mutate(putin = car::recode(Q87, "5=1; 4=0; 6=0; else = NA")) 

nd <-nd %>% 
  mutate(polknow = pence + pelosi + merkel + putin)
  

