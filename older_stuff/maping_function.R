library(socsci)
library(tidytext)
library(ggmap)

map <- read_csv("D://nd/all_geocodes.csv")


tidy_col <- map %>% 
  mutate(id = name) %>% 
  unnest_tokens(word, name)

col_w_den <- tidy_col %>% 
  filter(word %in% denom$word) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(type = "Denominational")

col_wo_den <- map %>% 
  filter(!name %in% col_w_den$id) %>% 
  mutate(type = "Non-Denominational")

type_col <- bind_rows(col_w_den, col_wo_den)



fun <- function(df, aaa){
  
  
  filename <- paste('D://nd/maps/', aaa, '.png', sep = '')
  
  map1 <- df %>% 
    filter(city == aaa)
  
   
   qmplot(lon, lat, data = map1, maptype = "toner-2011", color = type, shape = type, size = I(0.9), alpha=I(.7)) +
     facet_wrap(~ type) +
     scale_color_manual(values = c("steelblue1", "darkorange2")) +
     theme_gg("Josefin Sans") +
     theme(legend.position = "none") +
     labs(x = "", y = "", title = glue("{aaa}"), caption = "Data: Yellow Pages") +
     ggsave(filename, type = "cairo-png")
  
  
  
}


type_col %>% fun("Atlanta")
type_col %>% fun("Chicago")
type_col %>% fun("Columbus")
type_col %>% fun("Dallas")
type_col %>% fun("D.C.")
type_col %>% fun("Denver")
type_col %>% fun("Houston")
type_col %>% fun("Minneapolis")
type_col %>% fun("Miami")
type_col %>% fun("NYC")
type_col %>% fun("Philadelphia")
type_col %>% fun("Phoenix")
type_col %>% fun("Portland")
type_col %>% fun("San Francisco")
type_col %>% fun("Seattle")
type_col %>% fun("St. Louis")

type_col %>% 
  filter(city == "Philadelphia") %>% 
  leaflet(.) %>% 
  addTiles() %>% 
  addMarkers(~lon, ~lat, popup = ~name) 







