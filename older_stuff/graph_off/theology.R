fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>% 
    mutate(ques = car::recode(!! var, "1=5; 4=2; 3=3; 2=4; 5=1; else = NA")) %>% 
    group_by(nonden) %>% 
    ct(ques, wt = weight) %>% 
    mutate(group = name)
  
  
}

aaa1 <- nd %>% fun(q32_1, "The Devil Exists")
aaa2 <- nd %>% fun(q32_2, "There is Evil\nOut in the World")
aaa3 <- nd %>% fun(q32_3, "We Must Make Every Effort\nto Avoid Sinful People")
aaa4 <- nd %>% fun(q32_4, "The Bible is the\nliteral word of God")

graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = ques, y = pct, group = nonden, fill = nonden)) +
  geom_col(position = "dodge") +
  facet_wrap(~ group) +
  theme_gg("Josefin Sans") +
  scale_fill_manual(values = c("steelblue2", "darkorange2")) +
  y_pct() + 
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 3.5, family = "font") +
  scale_x_continuous(labels = c("Strongly\nDisagree", "Disagree", "Neither\nAgree or\nDisagree", "Agree", "Strongly\nAgree")) +
  labs(x = "", y = "", title = "Theological Items") +
  ggsave("D://nd/graph_off/theology.png", type = "cairo-png", width = 10, height = 8)


fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>% 
    filter(q16 == 1) %>% 
    mutate(ques = frcode({{var}} == 5 ~ "Strongly\nDisagree",
                         {{var}} == 4 ~ "Disagree",
                         {{var}} == 3 ~ "Neither\nAgree or\nDisagree",
                         {{var}} == 2 ~ "Agree",
                         {{var}} == 1 ~ "Strongly\nAgree")) %>% 
    group_by(nonden) %>% 
    ct(ques, wt = weight, show_na = FALSE) %>% 
    mutate(group = name)
  
  
}

aaa1 <- nd %>% fun(q32_1, "The Devil Exists")
aaa2 <- nd %>% fun(q32_2, "There is Evil\nOut in the World")
aaa3 <- nd %>% fun(q32_3, "We Must Make Every Effort\nto Avoid Sinful People")
aaa4 <- nd %>% fun(q32_4, "The Bible is the\nliteral word of God")

graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = ques, y = pct, group = nonden, fill = nonden)) +
  geom_col(position = "dodge", alpha = .7) +
  facet_wrap(~ group) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 3.5, family = "font") +
  labs(x = "", y = "", title = "", subtitle = "Only Born-Again Respondents") +
  ggsave("D://nd/graph_off/theology_ba.png", type = "cairo-png", width = 10, height = 8)