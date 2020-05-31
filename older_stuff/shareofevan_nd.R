

fun <- function(df, number){
  
  over <- df %>% 
    filter(year == 1982 | year == 1987) %>% 
    mutate(nn = case_when(denom == number ~ 1, TRUE ~ 0)) %>% 
    mutate(type = case_when(evangelical == 1  & nn == 0  ~ "Denom Evan",
                            evangelical == 1  & nn == 1  ~ "NonDenom Evang" )) %>% 
    group_by(year) %>% 
    ct(type, wt = oversamp, show_na = FALSE) 
  
  wtss <- df %>% 
    mutate(nn = case_when(denom == number ~ 1, TRUE ~ 0)) %>% 
    mutate(type = case_when(evangelical == 1  & nn == 0 ~ "Denom Evan",
                            evangelical == 1  & nn == 1 ~ "NonDenom Evang" )) %>% 
    group_by(year) %>% 
    ct(type, wt = wtssall, show_na = FALSE) 
  
  wtss <- wtss %>% 
    filter(year != 1982) %>% 
    filter(year != 1987)
  
  bind_rows(over, wtss) %>% 
    arrange(year)
  
}

aa1 <- gss %>% fun(70) 


aa1 %>% 
  filter(type == "NonDenom Evang") %>% 
  ggplot(., aes(x = year, y = pct, group = type)) +
  geom_point() +
  geom_line()


aa1 %>% 
  filter(type == "NonDenom Evang") %>% 
  ggplot(., aes(x = year, y = pct, color = type)) +
  geom_point(size=3, color="white") +
  geom_point(size=2, shape=1, alpha=.5) +
  geom_point(size=1, shape=19) +
  geom_smooth(linetype = "twodash", se = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  theme_gg("Josefin Sans") +
  scale_color_manual(values = c("#FF7E0D")) +
  labs(x = "", y = "Share of Evangelical Population", title = "Percent of Evangelicals Who Are Non-Denominational") +
  ggsave("D://nd/graph_off/shareofevan_nd.png", type = "cairo-png", width = 7)


