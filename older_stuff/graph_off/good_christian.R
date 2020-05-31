fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>%
    filter(q16 == 1) %>% 
    mutate(ques = !! var) %>% 
    group_by(nonden) %>% 
    ct(ques, wt = weight) %>% 
    mutate(group = name)
  
  
}



aaa1 <- nd %>% fun(q40_1, "Good Christian + Pro-Choice")
aaa2 <- nd %>% fun(q40_2, "Good Christian + Pro Death Penalty")
aaa3 <- nd %>% fun(q40_3, "Good Christian + Democrat")
aaa4 <- nd %>% fun(q40_4, "Good Christian + Republican")
aaa5 <- nd %>% fun(q40_5, "Good Christian + Pro SSM")



graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = ques, y = pct, group = nonden, fill = nonden)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ group) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .01, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 2.5, family = "font") +
  scale_x_continuous(labels = c("Strongly\nDisagree", "Disagree", "Neither\nAgree or\nDisagree", "Agree", "Strongly\nAgree")) +
  labs(x = "", y = "", title = "Good Christian Items", subtitle = "Only Born-Again Respondents") +
  ggsave("D://nd/graph_off/good_christian.png", type = "cairo-png", width = 12, height = 8)


fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>%
    filter(q16 == 1) %>% 
    mutate(ques = car::recode(!! var, "1:2 =1; 3=2; 4:5=3")) %>% 
    group_by(nonden) %>% 
    ct(ques, wt = weight) %>% 
    mutate(group = name)
  
  
}



aaa1 <- nd %>% fun(q40_1, "Good Christian + Pro-Choice")
aaa2 <- nd %>% fun(q40_2, "Good Christian + Pro Death Penalty")
aaa3 <- nd %>% fun(q40_3, "Good Christian + Democrat")
aaa4 <- nd %>% fun(q40_4, "Good Christian + Republican")
aaa5 <- nd %>% fun(q40_5, "Good Christian + Pro SSM")



graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = ques, y = pct, group = nonden, fill = nonden)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ group) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .03, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 3.5, family = "font") +
  scale_x_continuous(labels = c("Disagree", "Neither\nAgree or\nDisagree", "Agree")) +
  labs(x = "", y = "", title = "Good Christian Items", subtitle = "Only Born-Again Respondents") +
  ggsave("D://nd/graph_off/good_christian_3bars.png", type = "cairo-png", width = 12, height = 8)
