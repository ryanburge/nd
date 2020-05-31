nd <- nd %>% 
  mutate(religpew = q2) %>% 
  mutate(religpew_protestant = case_when(q3 == 1 ~ 3, 
                                         q3 == 17 ~ 1, 
                                         q3 == 4 ~ 2, 
                                         q3 == 6 ~ 4, 
                                         q3 == 7 ~ 5, 
                                         q3 == 8 ~ 6, 
                                         q3 == 9 ~ 7, 
                                         q3 == 10 ~ 8, 
                                         q3 == 11 ~ 9, 
                                         q3 == 12 ~ 10, 
                                         q3 == 13 ~ 11, 
                                         q3 == 14 ~ 12, 
                                         q3 == 15 ~ 13, 
                                         q3 == 3 | q3 == 2 ~ 90)) %>% 
  mutate(religpew_baptist = q4) %>% 
  mutate(religpew_methodist = q5) %>% 
  mutate(religpew_nondenom = q6) %>% 
  mutate(religpew_lutheran = q7) %>% 
  mutate(religpew_presby = q8) %>% 
  mutate(religpew_pentecost = q9) %>% 
  mutate(religpew_episcop = q10) %>% 
  mutate(religpew_christian = q11) %>% 
  mutate(religpew_congreg = q12) %>% 
  mutate(religpew_holiness = q13) %>% 
  mutate(religpew_reformed = q14) %>% 
  mutate(religpew_advent = q15) %>% 
  mutate(hiatt = case_when(q17 == 4 | q17 == 5 | q17 == 6 | q17 == 7 ~ 1, TRUE ~ 0)) %>% 
  mutate(white = case_when(q95_1 == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(black = case_when(q95_2 == 1 ~ 1, TRUE ~ 0))



## Baptist

nd <- nd %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

nd <- nd %>%
  mutate(abc = recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

nd <- nd %>%
  mutate(ibc = recode(religpew_baptist, "5=1; else=0")) 

nd <- nd %>%
  mutate(bgc = recode(religpew_baptist, "6=1; else=0")) 

nd <- nd %>%
  mutate(mbc = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

nd <- nd %>%
  mutate(cb = recode(religpew_baptist, "8=1; else=0")) 

nd <- nd %>%
  mutate(fwb = recode(religpew_baptist, "9=1; else=0")) 

nd <- nd %>%
  mutate(gabb = recode(religpew_baptist, "10=1; else=0")) 

nd <- nd %>%
  mutate(obc = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

nd <- nd %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
nd <- nd %>%
  mutate(fmc = recode(religpew_methodist, "2=1; else=0")) 

nd <- nd %>%
  mutate(omc = recode(religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

nd <- nd %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

nd <- nd %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

nd <- nd %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

nd <- nd %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

nd <- nd %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

nd <- nd %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
nd <- nd %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

nd <- nd %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

nd <- nd %>% 
  mutate(abc = recode(religpew_baptist, "2=1; 4=1; else=0"))

nd <- nd %>% 
  mutate(epis = recode(religpew_episcop, "1:90=1; else=0"))

nd <- nd %>% 
  mutate(luth = recode(religpew_lutheran, "1=1; 4=1; else=0"))

nd <- nd %>% 
  mutate(meth = recode(religpew_methodist, "1=1; 90=1; else=0"))

nd <- nd %>% 
  mutate(pres = recode(religpew_presby, "1=1; 90=1; else=0"))

nd <- nd %>% 
  mutate(cong = recode(religpew_congreg, "1=1; 3=1; 90=1; else=0"))

nd <- nd %>% 
  mutate(doc = recode(religpew_protestant, "8=1; else=0"))

nd <- nd %>% 
  mutate(reform = recode(religpew_protestant, "11=1; else=0"))

nd <- nd %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

nd <- nd %>% 
  mutate(meth = recode(religpew_methodist, "3:4=1; else=0"))

nd <- nd %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

nd <- nd %>% 
  mutate(nbap = recode(religpew_baptist, "3=1; else=0"))

nd <- nd %>%
  mutate(abc = recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

nd <- nd %>%
  mutate(miss = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

nd <- nd %>%
  mutate(obap = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

nd <- nd %>%
  mutate(ometh = recode(religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

nd <- nd %>% 
  mutate(apos = recode(religpew_pentecost, "6=1; 7=1; else=0"))

nd <- nd %>%
  mutate(open = recode(religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

nd <- nd %>%
  mutate(holy = recode(religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


nd <- nd %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

nd <- nd %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

nd <- nd %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

nd <- nd %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

nd <- nd %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))





           