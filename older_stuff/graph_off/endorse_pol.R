nd <- nd %>% mutate(nonden=car::recode(Q3, "1=1; 2:17=0"),
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

aaa1 <- nd %>% fun(q50_1, "Keep Out\nOf Political Matters")
aaa2 <- nd %>% fun(q50_2, "Endorse Candidates\nOr Parties")
aaa3 <- nd %>% fun(q50_3, "Voter\nRegistration\nDrives")
aaa4 <- nd %>% fun(q50_4, "Forums\nfor Politicians")
aaa5 <- nd %>% fun(q50_5, "Succeed or\nFail On Its Own")

graph <- bind_df("aaa")


graph %>% 
  ggplot(., aes(y=mean, x= group, color =nonden)) +
  geom_point(position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, width = 0) +
  coord_flip() +
  scale_y_continuous(breaks = c(1,2,3,4,5)) +
  theme_gg("Abel") +
  scale_color_tableau() +
  theme(legend.position = "bottom") +
  coord_flip(ylim = c(1,5), clip = 'off') +
  annotate("text", x=5, y = 5.65, label = "No Voter\nRegistration", size = 4, family = "font") +
  annotate("text", x=4, y = 5.65, label = "Support Good\nGovt. Practices", size = 4, family = "font") +
  annotate("text", x=3, y = 5.65, label = "Express\nPolitical Views", size = 4, family = "font") +
  annotate("text", x=2, y = 5.65, label = "Avoid Pol.\nDiscussion", size = 4, family = "font") +
  annotate("text", x=1, y = 5.65, label = "Not Express\nOpinion\non Candidates", size = 4, family = "font") +
  theme(plot.margin = margin(0, 85, 0, 0, unit = "pt")) +
  labs(x = "", y = "", title = "The Role of Church in Politics") +
  ggsave("D://nd/graph_off/endorse_pol.png", width = 8, height = 8)

