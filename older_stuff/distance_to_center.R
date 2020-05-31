library(geosphere)
library(spatialrisk)


geo_fun <- function(df, aaa, lat9, lon9){
  
  
   df %>% 
    filter(city == aaa) %>% 
    mutate(lat2 = lat9, lon2 = lon9) %>% 
    mutate(newcolumn_distance = spatialrisk::haversine(lat, lon, lat2, lon2)) %>% 
    mutate(distance = newcolumn_distance/1609)  %>% 
    group_by(type) %>% 
    mean_ci(distance, ci = .84) %>% 
    mutate(city = aaa)

  
}

hhh1 <- type_col %>% geo_fun("Chicago", 41.881832, -87.623177)
hhh2 <- type_col %>% geo_fun("Atlanta", 33.7490, -84.3880)
hhh3 <- type_col %>% geo_fun("Columbus", 	39.983334, -82.983330)
hhh4 <- type_col %>% geo_fun("Dallas", 32.779167, -96.808891)
hhh5 <- type_col %>% geo_fun("D.C", 38.9072, -77.0369)
hhh6 <- type_col %>% geo_fun("Denver", 39.7392, -104.9903)
hhh7 <- type_col %>% geo_fun("Houston", 29.7604, -95.3698)
hhh8 <- type_col %>% geo_fun("Minneapolis", 44.9778, -93.2650)
hhh9 <- type_col %>% geo_fun("Miami", 25.7617, -80.1918)
hhh10 <- type_col %>% geo_fun("NYC", 40.7128, -74.0060)
hhh11 <- type_col %>% geo_fun("Philadelphia", 39.9526, -75.1652)
hhh12 <- type_col %>% geo_fun("Phoenix", 33.4484, -112.0740)
hhh13 <- type_col %>% geo_fun("Portland", 45.5051, -122.6750)
hhh14 <- type_col %>% geo_fun("Seattle", 47.6062, -122.3321)
hhh15 <- type_col %>% geo_fun("St. Louis", 38.6270, -90.1994)

coord <- bind_df("hhh")

coord <- coord %>% 
  mutate(type = frcode(type == "Denominational" ~ "Denom.",
                       type == "Non-Denominational" ~ "Non-\nDenom."))

coord %>% 
  mutate(mean = round(mean, 1)) %>% 
  ggplot(., aes(x = type, y = mean, fill = type)) +
  geom_col(color = "black") +
  facet_wrap(~city, ncol = 5) +
  theme_gg("Josefin Sans") +
  geom_text(aes(y = 1.5, label = mean), position = position_dodge(width = .9), size =4, family = "font") +
  scale_fill_manual(values = c("steelblue1", "darkorange2")) +
  labs(x = "", y = "Distance to City Center (in Miles)", title = "Distance to City Center for Church Types", caption = "Data: Yellow Pages") +
  ggsave("D://nd/maps/distance.png", width = 9)