graph <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
              nonden=frcode(nonden==0 ~ "Denominational",
                            nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ct(q24, show_na = FALSE, wt = weight) %>% 
  mutate(size = frcode(q24 == 4 ~ "0-50",
                       q24 == 7 ~ "50-100",
                       q24 == 8 ~ "101-300",
                       q24 == 9 ~ "301-500",
                       q24 == 10 ~ "500-1000",
                       q24 == 11 ~ "1000+"))


dat_text <- data.frame(
  nonden = c("Denominational", "Non-Denominational"),
  label   = c(" 300 or More = 18.3%", "300 or More = 25.6%")
)



graph %>% 
  ggplot(., aes(x = size, y = pct, fill = nonden)) +
  geom_col(alpha = .7) + 
  facet_wrap(~ nonden, ncol = 1) +
  theme_gg("Josefin Sans") +
  scale_fill_tableau() + 
  y_pct() + 
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4.5, family = "font") +
  labs(x = "", y = "", title = "") +
  annotate("rect", xmin = 3.5, xmax = 6.6, ymin = 0, ymax = Inf, alpha = .2) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = -Inf, label = label), hjust = -2.15, vjust = -14, family = "font") +
  theme(plot.title = element_text(size = 12)) +
  ggsave("D://nd/graph_off/sizeofcong.png", type = "cairo-png", width = 6) 