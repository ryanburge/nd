fun <- function(df, var, name){
  var <- enquo(var)
  
  df %>%
    filter(q16 == 1) %>% 
    mutate(ques = !! var) %>% 
    group_by(nonden) %>% 
    ct(ques, wt = weight) %>% 
    mutate(group = name)
  
  
}



aaa1 <- nd %>% fun(q46_1, "An elected official who commits an immoral act\n in their personal life can still behave ethically\nand fulfill their duties in their public and professional life ")
aaa2 <- nd %>% fun(q46_2, "Donald Trump was anointed by God\nto become President of the United States")
aaa3 <- nd %>% fun(q46_3, "All Presidents of the United States\nhave been anointed by God")



graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = ques, y = pct, group = nonden, fill = nonden)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ group, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  scale_x_continuous(labels = c("Strongly\nDisagree", "Disagree", "Neither\nAgree or\nDisagree", "Agree", "Strongly\nAgree")) +
  labs(x = "", y = "", title = "God's Anointing", subtitle = "Only Born-Again Respondents") +
  ggsave("D://nd/graph_off/anointing.png", type = "cairo-png", width = 8, height = 12)