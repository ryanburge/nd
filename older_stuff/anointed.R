
nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q16 == 1) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  ct(ag, wt = weight)

nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q70_6 == 1) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  ct(ag, wt = weight)

nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q16 == 1) %>% 
  mutate(ag = frcode(q46_3 == 1 | q46_3 == 2 ~ "Disagree",
                     q46_3 == 3 ~ "Neither",
                     q46_3 == 4 | q46_3 == 5 ~ "Agree")) %>% 
  ct(ag, wt = weight)

nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q70_6 == 1) %>% 
  mutate(ag = frcode(q46_3 == 1 | q46_3 == 2 ~ "Disagree",
                     q46_3 == 3 ~ "Neither",
                     q46_3 == 4 | q46_3 == 5 ~ "Agree")) %>% 
  ct(ag, wt = weight)

nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q16 == 1) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  group_by(ag) %>% 
  mean_ci(q54_6, wt = weight) 

nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q70_6 == 1) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  group_by(ag) %>% 
  mean_ci(q54_6, wt = weight) 


aa1 <- nd %>% 
  filter(q95_1 == 1) %>% 
  mutate(attend = frcode(q17 == 2 ~ "Less than\nOnce a\nMonth", 
                         q17 == 3 ~ "Once\na Month", 
                         q17 == 4 ~ "2-3x\na Month",
                         q17 == 5 ~ "Once\na Week",
                         q17 == 6 | q17 == 7 ~ "More than\nOnce a Week")) %>% 
  group_by(attend) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  ct(ag) %>% 
  mutate(ques = "Trump was Anointed by God")

aa2 <- nd %>% 
  filter(q95_1 == 1) %>% 
  mutate(attend = frcode(q17 == 2 ~ "Less than\nOnce a\nMonth", 
                         q17 == 3 ~ "Once\na Month", 
                         q17 == 4 ~ "2-3x\na Month",
                         q17 == 5 ~ "Once\na Week",
                         q17 == 6 | q17 == 7 ~ "More than\nOnce a Week")) %>% 
  group_by(attend) %>% 
  mutate(ag = frcode(q46_3 == 1 | q46_3 == 2 ~ "Disagree",
                     q46_3 == 3 ~ "Neither",
                     q46_3 == 4 | q46_3 == 5 ~ "Agree")) %>% 
  ct(ag) %>% 
  mutate(ques = "All Presidents Anointed by God")

graph <- bind_df("aa")

graph %>% 
  filter(ag == "Agree") %>% 
  ggplot(., aes(x = attend, y = pct, fill = ques)) +
  geom_col(position = "dodge", color = "black") +
  theme_gg("Josefin Sans") +
  theme(legend.position = "bottom") +
  y_pct() +
  scale_fill_tableau() + 
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "Church Attendance", y = "", title = "Are Presidents Anointed by God?", subtitle = "Among White Protestants") +
  ggsave("E://anointed.png", type = "cairo-png")



aa1 <- nd %>% 
  filter(q95_1 == 1) %>% 
  mutate(pid3 = frcode(q27 == 1 | q27 == 2 | q27 == 3 ~ "Democrat",
                       q27 == 4 ~ "Independent",
                       q27 == 5 | q27 == 6 | q27 == 7 ~ "Republican")) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  group_by(pid3) %>% 
  ct(ag) %>% 
  mutate(ques = "Trump was Anointed by God")


aa2 <- nd %>% 
  filter(q95_1 == 1) %>% 
  mutate(pid3 = frcode(q27 == 1 | q27 == 2 | q27 == 3 ~ "Democrat",
                       q27 == 4 ~ "Independent",
                       q27 == 5 | q27 == 6 | q27 == 7 ~ "Republican")) %>% 
  mutate(ag = frcode(q46_3 == 1 | q46_3 == 2 ~ "Disagree",
                     q46_3 == 3 ~ "Neither",
                     q46_3 == 4 | q46_3 == 5 ~ "Agree")) %>% 
  group_by(pid3) %>% 
  ct(ag) %>% 
  mutate(ques = "All Presidents Were Anointed by God")
  

graph <- bind_df("aa")

graph %>% 
  filter(ag == "Agree") %>% 
  ggplot(., aes(x = pid3, y = pct, fill = pid3)) +
  geom_col(position = "dodge", color = "black") +
  facet_wrap(~ ques, ncol = 1) +
  theme_gg("Josefin Sans") +
  y_pct() +
  pid3_fill() +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "", title = "Are Presidents Anointed by God?", subtitle = "Among White Protestants") +
  ggsave("E://anointed_pid.png", type = "cairo-png", width = 5)





aa1 <- nd %>% 
  filter(q95_1 == 1) %>% 
  mutate(bible = frcode(q58 == 3 ~ "Not God's\nWord",
                        q58 == 2 ~ "Inspired, but\nNot Literal",
                        q58 == 1 ~ "Literal Word\nof God")) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  group_by(bible) %>% 
  ct(ag) %>% 
  mutate(ques = "Trump was Anointed by God")


aa2 <- nd %>% 
  filter(q95_1 == 1) %>% 
  mutate(bible = frcode(q58 == 3 ~ "Not God's\nWord",
                        q58 == 2 ~ "Inspired, but\nNot Literal",
                        q58 == 1 ~ "Literal Word\nof God")) %>% 
  mutate(ag = frcode(q46_3 == 1 | q46_3 == 2 ~ "Disagree",
                     q46_3 == 3 ~ "Neither",
                     q46_3 == 4 | q46_3 == 5 ~ "Agree")) %>% 
  group_by(bible) %>% 
  ct(ag) %>% 
  mutate(ques = "All Presidents Were Anointed by God")


graph <- bind_df("aa")

graph %>% 
  filter(bible != "NA") %>% 
  filter(ag == "Agree") %>% 
  ggplot(., aes(x = bible, y = pct, fill = bible)) +
  geom_col(position = "dodge", color = "black") +
  facet_wrap(~ ques, ncol = 1) +
  theme_gg("Josefin Sans") +
  y_pct() +
  scale_fill_tableau() +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "", title = "Are Presidents Anointed by God?", subtitle = "Among White Protestants") +
  ggsave("E://anointed_bible.png", type = "cairo-png", width = 5)


regg <- nd %>% 
  mutate(ag_all = car::recode(q46_3, "4:5=1; else =0")) %>% 
  mutate(ag_trump = car::recode(q46_2, "4:5=1; else =0")) %>% 
  mutate(rep = car::recode(q27, "5:7=1; else =0")) %>% 
  mutate(literal = car::recode(q58, "1=1; else =0")) %>% 
  mutate(white = car::recode(q95_1, "1=1; else=0"))

reg1 <- glm(ag_all ~ rep + literal + white, data = regg, family = "binomial")
reg2 <- glm(ag_trump ~ rep + literal + white, data = regg, family = "binomial")




nd %>% 
  filter(q95_1 == 1) %>% 
  filter(q16 == 1) %>% 
  mutate(ag = frcode(q46_2 == 1 | q46_2 == 2 ~ "Disagree",
                     q46_2 == 3 ~ "Neither",
                     q46_2 == 4 | q46_2 == 5 ~ "Agree")) %>% 
  mutate(approve = frcode(q42 == 1 | q42 == 2 ~ "Approve",
                          q42 == 3 | q42 == 4 ~ "Disapprove")) %>%
  group_by(ag) %>% 
  ct(approve, wt = weight)

