library(socsci)
library(car)
library(haven)
library(psych)
source("D://nd/theme.R")

nd <- read_csv("D://nd/data.csv") %>% 
  mutate(id = ResponseId)

weight <- read_dta("D://nd/rake_weights.dta") %>% 
  rename(id = responseid)

nd <- left_join(nd, weight, by="id")

nd <- nd %>% rename_all(tolower)
nd <- nd %>% filter(gc==1)


#Denominations####
nd <- nd %>% mutate(relig=frcode(q2==1 ~ "Non-Denominational",
                                 q2==16 ~ "Protestant",
                                 q2==18 ~ "Other Christian"))

nd <- nd %>% mutate(denfam=frcode(q3==1 ~ "Non-Denominational",
                                  q3==17 ~ "Baptist",
                                  q3==4 ~ "Methodist",
                                  q3==6 ~ "Lutheran",
                                  q3==7 ~ "Prebysterian",
                                  q3==8 ~ "Pentecostal",
                                  q3==9 ~ "Episcopalian",
                                  q3==10 ~ "Churches of Christ",
                                  q3==11 ~ "Congregational",
                                  q3==12 ~ "Holiness",
                                  q3==13 ~ "Reformed",
                                  q3==14 ~ "Adventist",
                                  q3==15 ~ "Jehovahs Witness",
                                  q3==16 ~ "Other"))
#Non-Denom Dummy
nd <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                    nonden=frcode(nonden==0 ~ "Denominational",
                                  nonden==1 ~ "Non-Denominational"))

#Age in years
nd <- nd %>% mutate(age=2019-q90_1)

nd <- nd %>% mutate(age4=car::Recode(age, "19:24=1; 25:44=2; 45:64=3; 65:110=4"))

#Gender
nd <- nd %>% mutate(Women=frcode(q89==1 ~ "Men",
                                 q89==2 ~ "Women"))

#College
nd <- nd %>% mutate(ed=q96, college=car::recode(q96, "1:4=0; 5:6=1"))

#Civic Skills
nd <- nd %>% mutate_at(., vars(starts_with('q92_')), funs(replace_na(., 0)))
nd <- nd %>% mutate(skills=q92_1+q92_2+q92_3+q92_4)
nd <- nd %>% mutate(skillsc=car::recode(skills, "0=0; 1:4=1"))

#Race Dummies
nd <- nd %>% mutate(white=car::recode(q95_1, "NA=0"),
                    black=car::recode(q95_3, "NA=0"),
                    hispanic=car::recode(q95_4, "NA=0"))

#Other demographics
nd <- nd %>% mutate(educ=q96,
                    samecomm=q99,
                    income=q100)
nd <- nd %>% mutate(straight=car::recode(q97, "1=1; 2:4=0"))

#Religious Authority
nd %>% select(q33_1, q33_2, q33_3, q33_4, q33_5) %>% alpha(.) #a=.8
nd <- nd %>% mutate(relauth=1-((q33_1+q33_2+ q33_3+ q33_4+ q33_5)-5)/20)

#Evil in the world
nd <- nd %>% mutate(evil=1-((q32_1+q32_2+q32_3)-3)/12) #higher is agree more evil

#Prosperity Gospel
nd %>% select(q31_1, q31_2, q31_3, q31_4, q31_5) %>% alpha(.) #a=.79
nd <- nd %>% mutate(prosperity=1-((q31_1+q31_2+ q31_3+ q31_4+ q31_5)-5)/20)

#Pastoral leadership####
nd <- nd %>% mutate(q61_1=car::Recode(q61_1,"4=3; 6=4; 7=5"),
                    q61_2=car::Recode(q61_2,"4=3; 6=4; 7=5"),
                    q61_3=car::Recode(q61_3,"4=3; 6=4; 7=5"))
nd %>% select(q61_1, q61_2, q61_3) %>% alpha(.) #a=.71
nd <- nd %>% mutate(clergylead=1-((q61_1+q61_2+q61_3)-3)/12) #higher is more leadery

#Race of Congregation
nd <- nd %>% mutate(interracial=car::recode(q23_1, "0:50=1; 51:100=0"))
nd <- nd %>% mutate(interracial60=car::recode(q23_1, "0:60=1; 61:100=0"))

#Church Size
nd <- nd %>% mutate(size=as.factor(q24))
nd <- nd %>% mutate(size=frcode(size==4 ~ "0-50", 
                                size==7 ~ "50-100",  
                                size==8 ~ "101-300", 
                                size==9 ~ "301-500", 
                                size==10 ~ "500-1000", 
                                size==11 ~ "1000+"))  

#Religious Knowledge####
nd <- nd %>% 
  filter(q77 != "NA") %>% 
  mutate(sabbath = car::recode(q77, "4=1; 5:6=0; else = NA")) 

nd <- nd %>% 
  filter(q78 != "NA") %>% 
  mutate(disciples = car::recode(q78, "5=1; 4=0; 6=0; else = NA")) 

nd <- nd %>% 
  filter(q79 != "NA") %>% 
  mutate(ramadan = car::recode(q79, "3=1; 1:2=0; else = NA")) 

nd <- nd %>% 
  filter(q80 != "NA") %>% 
  mutate(moses = car::recode(q80, "6=1; 4:5=0; else = NA")) 

nd <- nd %>% 
  filter(q81 != "NA") %>% 
  mutate(gfriday = car::recode(q81, "5=1; 4=0; 6=0; else = NA")) 

nd <- nd %>% 
  filter(q82 != "NA") %>% 
  mutate(smith = car::recode(q82, "6=1; 4:5=0; else = NA")) 

nd <-nd %>% 
  mutate(relknow = sabbath + disciples + ramadan + moses + gfriday + smith)

#Political Knowledge ####
nd <- nd %>% 
  filter(q84 != "NA") %>% 
  mutate(pence = car::recode(q84, "6=1; 5=0; 7=0; else = NA")) 

nd <- nd %>% 
  filter(q85 != "NA") %>% 
  mutate(pelosi = car::recode(q85, "6=1; 4:5=0; else = NA")) 

nd <- nd %>% 
  filter(q86 != "NA") %>% 
  mutate(merkel = car::recode(q86, "4=1; 5:6=0; else = NA")) 

nd <- nd %>% 
  filter(q87 != "NA") %>% 
  mutate(putin = car::recode(q87, "5=1; 4=0; 6=0; else = NA")) 

nd <-nd %>% 
  mutate(polknow = pence + pelosi + merkel + putin)

#Polarization####
nd <- nd %>% mutate(polarized=abs(q29_1-q29_2))

#Service Refusal Experiment####
nd <- nd %>% gather(key="t_service", value="service", q35, q36, q37, q38, q39, na.rm=TRUE)

#Dump Approval
nd <- nd %>% mutate(trumpapp=car::recode(q42, "1:2=1; 3:4=0"))

#Political Participation####
nd <- nd %>% mutate(q47_1=car::recode(q47_1, "2=1;1=0"),
                    q47_2=car::recode(q47_2, "2=1;1=0"),
                    q47_3=car::recode(q47_3, "2=1;1=0"),
                    q47_4=car::recode(q47_4, "2=1;1=0"),
                    q47_5=car::recode(q47_5, "2=1;1=0"),
                    q47_6=car::recode(q47_6, "2=1;1=0"),
                    q47_8=car::recode(q47_8, "2=1;1=0"),
                    q47_9=car::recode(q47_9, "2=1;1=0"),
                    q47_10=car::recode(q47_10, "2=1;1=0"),
                    q47_11=car::recode(q47_11, "2=1;1=0"))
nd <- nd %>% mutate(polpar=q47_1+q47_2+q47_3+q47_4+q47_5+q47_6+q47_8+q47_9+q47_10+q47_11)

#Altruism and Experiment####
nd <- nd %>% 
  mutate(q48_1=car::recode(q48_1, "2=1;1=0"),
         q48_2=car::recode(q48_2, "2=1;1=0"),
         q48_3=car::recode(q48_3, "2=1;1=0"),
         q48_4=car::recode(q48_4, "2=1;1=0"),
         q48_5=car::recode(q48_5, "2=1;1=0"),
         q48_6=car::recode(q48_6, "2=1;1=0"),
         q48_7=car::recode(q48_7, "2=1;1=0"),
         q48_8=car::recode(q48_8, "2=1;1=0"),
         q48_9=car::recode(q48_9, "2=1;1=0"),
         q48_10=car::recode(q48_10, "2=1;1=0"))

nd <- nd %>% mutate(altruism=rowSums(select(., starts_with("q48_")), na.rm = TRUE))

nd <- nd %>% mutate(t_altruism=car::recode(altruism_do_q48, "1=1;2=0"), 
                    t_altruism=as.factor(t_altruism)) #list of actions first=1

#Democratic Norms#### 
nd %>% select(q52_1, q52_2, q52_4, q52_5) %>% alpha(.) #a=.4 doesn't hang together

#Christian Nationalism####
nd %>% select(q59_1, q59_2, q59_3, q59_4, q59_5) %>% alpha(., check.keys=TRUE) #a=.74
nd <- nd %>% 
  mutate(q59_1=car::recode(q59_1,"4=3;6=4;7=5"),
         q59_2=car::recode(q59_2,"4=3;6=4;7=5"),
         q59_3=car::recode(q59_3,"4=3;6=4;7=5"),
         q59_4=car::recode(q59_4,"4=3;6=4;7=5"),
         q59_5=car::recode(q59_5,"4=3;6=4;7=5"),
         q59_6=car::recode(q59_6,"4=3;6=4;7=5"))

#higher is more CN and 0-1 scale.
nd <- nd %>% mutate(cn1=((((6-q59_1)+(6-q59_2)+q59_3+(6-q59_4)+(6-q59_5)+(6-q59_6))-6)/24))

#Clergy Speech####
nd <- nd %>% mutate_at(., vars(starts_with('q60_')), funs(replace_na(., 0)))
nd <- nd %>% mutate(speech=q60_1+q60_3+q60_4+q60_5+q60_6+q60_7+q60_8++q60_9+q60_10+q60_11+q60_12+q60_18)
nd %>% ct(speech)
nd <- nd %>% mutate(speech7=car::recode(speech, "7:12=7")) #collapsed the top end

#Clergy Political Leadership####
nd %>% select(q61_1, q61_2,q61_3) %>% alpha(.) #a=.71
nd <- nd %>% mutate(
  q61_1=car::recode(q61_1, "4=3; 6=4; 7=5"),
  q61_2=car::recode(q61_2, "4=3; 6=4; 7=5"),
  q61_3=car::recode(q61_3, "4=3; 6=4; 7=5"))
#higher is agree and collapsed to 0-1
nd <- nd %>% mutate(clergylead=((12-((q61_1+q61_2+q61_3)-3))/12))

#Child-rearing authority####
nd <- nd %>% mutate(authority=(q64_1-1)+(q64_2-1)+(q64_3-1))

#Deliberative Values####
nd <- nd %>% mutate(deliberative=((20-(q65_1+q65_2+q65_3+q65_4+q65_5))/15))

#Views Compared to Congregation####
nd <- nd %>% mutate(issuediffsdir=(q68_1+q68_2+q68_3+q68_4+q68_5)/5)

nd <- nd %>% mutate(
  diff_1=car::recode(q68_1, "1=2; 2=1;3=0;4=1;5=2"),
  diff_2=car::recode(q68_2, "1=2; 2=1;3=0;4=1;5=2"),
  diff_3=car::recode(q68_3, "1=2; 2=1;3=0;4=1;5=2"),
  diff_4=car::recode(q68_4, "1=2; 2=1;3=0;4=1;5=2"),
  diff_5=car::recode(q68_5, "1=2; 2=1;3=0;4=1;5=2"), 
  issuediffs=(diff_1+diff_2+diff_3+diff_4+diff_5)/5)

#Putting together religious ID labels####
nd <- nd %>% mutate_at(., vars(starts_with('q70_')), funs(replace_na(., 0)))

#Religion and democracy####
nd %>% select(q71_1, q71_2, q71_3) %>% alpha(.) #a=.67

#higher is more supportive of rel-dem link, 0-1 scale
nd <- nd %>% mutate(reldem=1-(((q71_1+q71_2+q71_3)-3)/18))

#Fox News####
nd <- nd %>% mutate(fox=car::Recode(q75, "4=1; else=0"))


#You say rename, I say mutate, so the original varnames are still there####
nd <- nd %>% 
  mutate(baptist = q4) %>% 
  mutate(methodist = q5) %>% 
  mutate(nd = q6) %>% 
  mutate(lutheran = q7) %>% 
  mutate(presby = q8) %>% 
  mutate(pente = q9) %>% 
  mutate(epis = q10) %>% 
  mutate(christian = q11) %>% 
  mutate(cong = q12) %>% 
  mutate(holy = q13) %>% 
  mutate(reformed = q14) %>% 
  mutate(advent = q15) %>% 
  mutate(bagain = q16) %>% 
  mutate(attend = q17) %>% 
  mutate(congsize = q24) %>% 
  mutate(pid7 = q27) %>% 
  mutate(ideo5 = q28) %>% 
  mutate(demprim = q44) %>% 
  mutate(repprim = q45) %>% 
  mutate(imm_therm = q54_1) %>% 
  mutate(muslim_therm = q54_2) %>% 
  mutate(likeme_therm = q54_3) %>% 
  mutate(gop_therm = q54_4) %>% 
  mutate(dem_therm = q54_5) %>% 
  mutate(djt_therm = q54_6) %>% 
  mutate(hrc_therm = q54_7) %>% 
  mutate(gay_therm = q54_8) %>% 
  mutate(xtnfun_therm = q54_9) %>% 
  mutate(atheist_therm = q54_10) %>% 
  mutate(racist_therm = q54_11) %>% 
  mutate(bible = q58) %>% 
  mutate(commsize = q67) %>% 
  mutate(relimp = q72) %>% 
  mutate(gender = q89) %>% 
  mutate(birthyr = q90_1) %>% 
  mutate(state = q94) %>% 
  mutate(white = q95_1) %>% 
  mutate(white = replace_na(white, 0)) %>% 
  mutate(hispanic = q95_2) %>% 
  mutate(hispanic = replace_na(hispanic, 0)) %>% 
  mutate(black = q95_3) %>% 
  mutate(black = replace_na(black, 0)) %>% 
  mutate(asian = q95_4) %>% 
  mutate(asian = replace_na(asian, 0)) %>% 
  mutate(other = q95_5) %>% 
  mutate(other = replace_na(other, 0)) %>% 
  mutate(race = frcode(white == 1 ~ 1,
                       black == 1 ~ 2, 
                       hispanic == 1 ~ 3, 
                       asian == 1 ~ 4,
                       other == 1 ~ 5)) %>% 
  mutate(educ = q96) %>% 
  mutate(gay = q97) %>% 
  mutate(marital = q98) %>% 
  mutate(moved = q99) %>% 
  mutate(income = q100)

nd$q66 <- gsub("percent", "", nd$q66)
nd$q66 <- gsub("%", "", nd$q66)
nd$q66 <- gsub("T", "", nd$q66)
nd$q66 <- as.numeric(nd$q66)






