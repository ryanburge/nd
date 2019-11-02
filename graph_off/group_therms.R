fun <- function(df, var1, name){
  
  var1 <- enquo(var1)
  
  
  a1 <-  df %>% 
    filter(q16 == 1) %>% 
    filter(!!var1 != "NA") %>% 
    group_by(nonden) %>% 
    mean_ci(!! var1) %>% 
    mutate(group = name)
  
  
}

aaa1 <-  nd %>% fun(q54_1, "Immigrants")
aaa2 <-  nd %>% fun(q54_2, "Muslims")
aaa3 <-  nd %>% fun(q54_3,  "People Like Me")
aaa4 <-  nd %>% fun(q54_4,  "Republicans")
aaa5 <-  nd %>% fun(q54_5,  "Democrats")
aaa6 <-  nd %>% fun(q54_6,  "Donald Trump")
aaa7 <-  nd %>% fun(q54_7,  "Hillary Clinton")
aaa8 <-  nd %>% fun(q54_8,  "Homosexuals")
aaa9 <-  nd %>% fun(q54_9,  "Christian Fundamentalists")
aaa10 <- nd %>% fun(q54_10, "Atheists")
aaa11 <- nd %>% fun(q54_11, "White Supremacists")

graph <- bind_df("aaa")



graph %>% 
  ggplot(., aes(y=mean, x= reorder(group, mean), color =nonden)) +
  geom_point(position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, width = 0) +
  coord_flip() +
  theme_gg("Abel") +
  scale_color_tableau() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", title = "Feeling Thermometers", subtitle = "Only Born-Again Respondents") +
  ggsave("D://nd/graph_off/group_therms.png", width = 8, height = 8)