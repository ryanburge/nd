nd <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                    nonden=frcode(nonden==0 ~ "Denominational",
                                  nonden==1 ~ "Non-Denominational"))

fun <- function(df, var1, name){
  
  var1 <- enquo(var1)
  
  
  a1 <-  df %>% 
    filter(q16 == 1) %>% 
    filter(!!var1 != "NA") %>% 
    group_by(nonden) %>% 
    mean_ci(!! var1) %>% 
    mutate(group = name)
  
  
}



aaa1 <- nd %>% fun(q33_1, "Clergy Out of the\nWay of the Congregration")
aaa2 <- nd %>% fun(q33_2, "Congregation Constructs\nTheir Own Salvation")
aaa3 <- nd %>% fun(q33_3, "Gospel is What the\nCongregation Makes of It")
aaa4 <- nd %>% fun(q33_4, "Many Valid Interpretations\nof the Bible")
aaa5 <- nd %>% fun(q33_5, "Church Must Adapt\nto Post-Modern Culture")


graph <- bind_df("aaa")



graph %>% 
  ggplot(., aes(y=mean, x= reorder(group, mean), color =nonden)) +
  geom_point(position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, width = 0) +
  coord_flip() +
  theme_gg("Abel") +
  scale_color_tableau() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", title = "Religious Authority", subtitle = "Only Born-Again Respondents") +
  scale_y_continuous(limits = c(1,5), labels = c("Strongly\nDisagree", "Disagree", "Neither\nAgree or\nDisagree", "Agree", "Strongly\nAgree")) +
  ggsave("D://nd/graph_off/rav.png", width = 8, height = 8)


nd <- nd %>% mutate(relauth=1-((q33_1+q33_2+ q33_3+ q33_4+ q33_5)-5)/20)

nd %>% 
  filter(q16 == 1) %>% 
   group_by(nonden) %>% 
    mean_ci(relauth)