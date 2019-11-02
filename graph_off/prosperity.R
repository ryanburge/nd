fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>% 
    mutate(ques = car::recode(!! var, "1=5; 4=2; 3=3; 2=4; 5=1; else = NA")) %>% 
    group_by(nonden) %>% 
    ct(ques, wt = weight) %>% 
    mutate(group = name)
  
  
}

aaa1 <- nd %>% fun(q31_1, "Reward Faith\nw/Health + Wealth")
aaa2 <- nd %>% fun(q31_2, "Sacrifices to God\nWill Be Rewarded")
aaa3 <- nd %>% fun(q31_3, "God Will Give You\nWhat You Seek if Faithful")
aaa4 <- nd %>% fun(q31_4, "Health + Wealth Are\nSigns of Anointing")
aaa5 <- nd %>% fun(q31_5, "Poverty + Sickness Signs\nof Lack of Faith")

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
  labs(x = "", y = "", title = "Prosperity Gospel Items") +
  ggsave("D://nd/graph_off/prosperity.png", type = "cairo-png", width = 10, height = 8)


fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>% 
    mutate(ques = car::recode(!! var, "1=5; 4=2; 3=3; 2=4; 5=1; else = NA")) %>% 
    group_by(nonden) %>% 
    mean_ci(ques, wt = weight) %>% 
    mutate(group = name)
  
  
}

aaa1 <- nd %>% fun(q31_1, "Reward Faith\nw/Health + Wealth")
aaa2 <- nd %>% fun(q31_2, "Sacrifices to God\nWill Be Rewarded")
aaa3 <- nd %>% fun(q31_3, "God Will Give You\nWhat You Seek if Faithful")
aaa4 <- nd %>% fun(q31_4, "Health + Wealth Are\nSigns of Anointing")
aaa5 <- nd %>% fun(q31_5, "Poverty + Sickness Signs\nof Lack of Faith")

graph <- bind_df("aaa")


graph %>% 
  # filter(group == "18-35" | group == "45-54" | group == "75 and Older")  %>% 
  ggplot(., aes(y=mean, x= group, color =nonden)) +
  geom_point(position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, width = 0) +
  coord_flip() +
  theme_gg("Abel") +
  scale_color_tableau() +
  theme(legend.position = "bottom") +
  ggsave("D://nd/graph_off/prosperity_means.png")

library(corrplot)

cor <- nd %>% 
  select(q31_1, q31_2, q31_3, q31_4, q31_5) 

cor <- cor(cor)

corrplot(cor, method = "number", type = "upper")