library(socsci)
library(tidytext)
library(ggmap) ## Grab the GitHub version, the version on CRAN won't load any new maptypes

map <- read_csv("D://nd/all_geocodes.csv")
denom <- read_csv("D://yp_scrapes/denom.csv")

tidy_col <- map %>% 
  mutate(id = name) %>% 
  unnest_tokens(word, name)

col_w_den <- tidy_col %>% 
  filter(word %in% denom$word) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(type = "Denom. Identifier")

col_wo_den <- map %>% 
  filter(!name %in% col_w_den$id) %>% 
  mutate(type = "No Denom. Identifier")

type_col <- bind_rows(col_w_den, col_wo_den)



map1 <- type_col %>% 
    filter(city == "Atlanta")
  
  
qmplot(lon, lat, data = map1,  maptype = "toner-2011", color = type, shape = type, size = I(0.9), alpha=I(.7)) +
  facet_wrap(~ type) +
  scale_color_manual(values = c("steelblue1", "darkorange2")) +
  theme_gg("Josefin Sans") +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://nd/Ch1/images/fig6.png", type = "cairo-png")


  