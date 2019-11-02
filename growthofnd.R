
fun <- function(df, number, name){
  
  over <- df %>% 
    filter(year == 1982 | year == 1987) %>% 
    mutate(nn = case_when(denom == number ~ 1, TRUE ~ 0)) %>% 
    group_by(year) %>% 
    mean_ci(nn, wt = oversamp) %>% 
    mutate(denom = name)
  
  wtss <- df %>% 
    mutate(nn = case_when(denom == number ~ 1, TRUE ~ 0)) %>% 
    group_by(year) %>% 
    mean_ci(nn, wt = wtssall) %>% 
    mutate(denom = name)
  
  wtss <- wtss %>% 
    filter(year != 1982) %>% 
    filter(year != 1987)
  
  bind_rows(over, wtss) %>% 
    arrange(year)
  
}

aa1 <- gss %>% fun(70, "Non-Denom")
aa2 <- gss %>% fun(14, "So. Baptist")
aa3 <- gss %>% fun(22, "United Meth.")

graph <- bind_df("aa")

graph %>% 
  filter(mean > 0) %>% 
  ggplot(., aes(x = year, y = mean, group = denom, color = denom)) +
  geom_point(size=3, color="white") +
  geom_point(size=2, shape=1, alpha=.5) +
  geom_point(size=1, shape=19) +
  geom_smooth(linetype = "twodash", se = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  theme_gg("Josefin Sans") +
  scale_color_manual(values = c('Non-Denom'= "#FF7E0D", 'So. Baptist' = "#4E79A7", 'United Meth.' = "#E15759")) +
  add_text(x = 2009, y = .11, word = "Non-Denom.", sz = 4) +
  add_text(x = 2014, y = .067, word = "So. Baptist", sz = 4) +
  add_text(x = 2011, y = .0485, word = "United Meth.", sz = 4) +
  labs(x = "", y = "Share of Total Population", title = "The Change in Size of Major Protestant Traditions") +
  ggsave("D://nd/graph_off/gss_trends.png", type = "cairo-png", width = 7)


graph <- gss %>% 
  filter(denom != 0) %>% 
  mutate(nn = frcode(denom == 70 ~ "Non-Denominational", TRUE ~ "Denominational")) %>% 
  group_by(year) %>% 
  ct(nn, wt = wtssall)

graph %>% 
  ggplot(., aes(x = 1, y = pct, fill = fct_rev(nn))) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ year, ncol =1, strip.position = "left") +
  scale_fill_manual(values = c('Non-Denominational' = "#FF7E0D", 'Denominational' = "#49B7FC")) +
  theme_gg("Josefin Sans") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y = element_text(angle = 180)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse=T)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 4, family = "font") +
  labs(x = "", y = "", title = "Share of Protestants that Are Non-Denominational", subtitle = "", caption = "") +
  ggsave("D://nd/graph_off/gss_stackedbars.png", type = "cairo-png", height = 10, width = 10)

graph %>% 
  ggplot(., aes(x = year, y = pct, fill = nn)) +
  geom_area(color = "black") +
  theme_gg("Josefin Sans") +
  scale_y_continuous(labels = percent) +
  xlim(1969, 2021) +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  theme(legend.position = "bottom") +
  add_text(x = 2020, y = .31, word = "77.1%", sz = 5) +
  add_text(x = 2020.2, y = .87, word = "22.9%", sz = 5) +
  add_text(x = 1969.5, y = .31, word = "96.6%", sz = 5) +
  add_text(x = 1970, y = .985, word = "3.4%", sz = 5) +
  scale_fill_manual(values = c('Non-Denominational' = "#FF7E0D", 'Denominational' = "#49B7FC")) +
  labs(x = "", y = "", title = "Share of Protestants that Are Non-Denominational", subtitle = "", caption = "") +
  ggsave("D://nd/graph_off/gss_area_graph.png", type = "cairo-png", height = 6, width = 8)



graph <- gss %>% 
  filter(race == 1) %>% 
  filter(attend ==7 | attend == 8) %>% 
  mutate(pid3 = frcode(partyid == 0 | partyid == 1 | partyid == 2 ~ "Democrat",
                       partyid == 3 ~ "Independent",
                       partyid == 4 | partyid == 5 | partyid == 6 ~ "Republican")) %>% 
  group_by(year) %>% 
  ct(pid3, wt = wtssall, show_na = FALSE)


graph %>% 
  ggplot(., aes(x = year, y = pct, fill = pid3)) +
  geom_area(color = "black", alpha = .7) +
  theme_gg("Abel") +
  scale_y_continuous(labels = percent) +
  xlim(1972, 2018) +
  theme(legend.position = "bottom") +
  pid3_fill() +
  add_text(x = 2016.5, y = .31, word = "62%", sz = 5) +
  add_text(x = 2016.2, y = .66, word = "15.5%", sz = 5) +
  add_text(x = 2016.2, y = .87, word = "22.5%", sz = 5) +
  add_text(x = 1974, y = .16, word = "32.6%", sz = 5) +
  add_text(x = 1974, y = .434, word = "10.5%", sz = 5) +
  add_text(x = 1974, y = .75, word = "56.9%", sz = 5) +
  labs(x = "", y = "", title = "The Political Partisanship of White Weekly Church Goers", subtitle = "", caption = "@ryanburge\nData: GSS 1972-2018") +
  ggsave("E://white_weekly_gss_area.png", type = "cairo-png", height = 6, width = 8)

