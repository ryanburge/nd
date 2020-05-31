fun <- function(df, number, grp){
  
  over <- df %>% 
    filter(year == 1982 | year == 1987) %>% 
    mutate(nn = case_when(denom == number ~ 1, TRUE ~ 0)) %>% 
    group_by(year) %>% 
    mean_ci(nn, wt = oversamp) 
  
  wtss <- df %>% 
    mutate(nn = case_when(denom == number ~ 1, TRUE ~ 0)) %>% 
    group_by(year) %>% 
    mean_ci(nn, wt = wtssall) 
  
  wtss <- wtss %>% 
    filter(year != 1982) %>% 
    filter(year != 1987)
  
  bind_rows(over, wtss) %>% 
    arrange(year) %>% 
    filter(mean > 0) %>% 
    mutate(group = grp)
  
}

## Denoms ####
eee1 <- gss %>% fun(14, "SBC")
eee2 <- gss %>% fun(22, "UMC")
eee3 <- gss %>% fun(70, "ND")

## Mainline ####
over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  group_by(year) %>% 
  mean_ci(mainline, wt = oversamp)

wtss <- gss %>% 
  group_by(year) %>% 
  mean_ci(mainline, wt = wtssall)

wtss <- wtss %>% 
  filter(year != 1982) %>% 
  filter(year != 1987)

eee4 <- bind_rows(over, wtss) %>% 
  mutate(group = "Mainline")

## WEP ####

over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  mutate(wep = case_when(evangelical == 1 & race == 1 ~ 1, TRUE ~ 0)) %>% 
  group_by(year) %>% 
  mean_ci(wep, wt = oversamp)

wtss <- gss %>% 
  mutate(wep = case_when(evangelical == 1 & race == 1 ~ 1, TRUE ~ 0)) %>% 
  group_by(year) %>% 
  mean_ci(wep, wt = wtssall)

wtss <- wtss %>% 
  filter(year != 1982) %>% 
  filter(year != 1987)

eee5 <- bind_rows(over, wtss) %>% 
  mutate(group = "White Evangelical")

graph <- bind_df("eee")


graph %>% 
  ggplot(., aes(x = year, y = mean, color = group, group = group)) +
  geom_point(size=3, color="white") +
  geom_point(size=2, shape=1, alpha=.5) +
  geom_point(size=1, shape=19) +
  geom_smooth(linetype = "twodash", se = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  theme_gg("Josefin Sans") +
  add_text(x = 1978, y = .29, word = "Mainline", sz = 4) +
  add_text(x = 1993, y = .26, word = "White Evan.", sz = 4) +
  add_text(x = 1978, y = .045, word = "Non-Denom.", sz = 4) +
  add_text(x = 1993, y = .105, word = "SBC", sz = 4) +
  add_text(x = 2003, y = .04, word = "UMC", sz = 4) +
  scale_color_manual(values = c("#59A14F","darkorange2", "#E05759", "#4E79A7", "#EDC947")) +
  labs(x = "", y = "Share of the Population", title = "Changes in Religious Tradition") +
  ggsave("D://nd/graph_off/fivetrads.png", type = "cairo-png", width = 7)



