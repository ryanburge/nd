
rfun <- function(df, var, name){
  
  var <- enquo(var)

  df %>% 
    mutate(vv = car::recode(!! var, "2=1; 1=0; else = 0")) %>% 
    rename(!!quo_name(name) := vv)
    

  
}


nd <- nd %>% rfun(q48_1, "blood")
nd <- nd %>% rfun(q48_2, "food")
nd <- nd %>% rfun(q48_3, "money1")
nd <- nd %>% rfun(q48_4, "cutline")
nd <- nd %>% rfun(q48_5, "vol")
nd <- nd %>% rfun(q48_6, "seat")
nd <- nd %>% rfun(q48_7, "plants")
nd <- nd %>% rfun(q48_8, "carry")
nd <- nd %>% rfun(q48_9, "borrow")
nd <- nd %>% rfun(q48_10, "money2")

nd <- nd %>% 
  mutate(attend = car::recode(q17, "7=6")) %>% 
  mutate(alt = blood + food + money1 + cutline + vol + seat + plants + carry + borrow + money2)  %>% 
  mutate(nonden=car::recode(q3, "1=1; 2:17=0")) %>% 
  mutate(nonden=frcode(nonden==0 ~ "Denominational", 
                       nonden==1 ~ "Non-Denominational")) %>% 
  mutate(educ=q96, samecomm=q99, income=q100, pid7 = q27)  %>% 
  mutate(ba = frcode(q16 == 1  ~ "Born-Again",
                     q16 == 2 ~ "Not Born-Again")) %>% 
  mutate(gender = car::recode(q89, "1=1; 2=0")) %>% 
  mutate(age=2019-q90_1)

regg <- nd %>% 
  filter(q16 == 1)

reg1 <- lm(alt ~ attend*nonden + educ + income + pid7 + gender + age, data = regg)

gg2 <- interact_plot(reg1, pred = attend, modx = nonden, interval = TRUE, int.width = .76)


gg2 + 
  scale_x_continuous(labels = c("Less than\nOnce a Month", "Once\na Month", "2-3x\na Month", "Once\na Week", "2-3x\na Week")) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  labs(x = "Church Attendance", y = "Number of Altruistic Acts", title = "Impact of Attendance on Altruism") +
  ggsave("D://nd/graph_off/altruism_interact_ba.png", width = 8) 
