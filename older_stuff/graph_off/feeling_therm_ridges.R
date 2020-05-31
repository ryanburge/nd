dem <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                     nonden=frcode(nonden==0 ~ "Denominational",
                                   nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  mean_ci(q29_1) %>% 
  na.omit() %>% 
  mutate(group = "Democrats")

rep <- nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
                     nonden=frcode(nonden==0 ~ "Denominational",
                                   nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  mean_ci(q29_2) %>% 
  na.omit() %>% 
  mutate(group = "Republicans")



nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
              nonden=frcode(nonden==0 ~ "Denominational",
                            nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ggplot(., aes(x = q29_1, y = nonden)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  # scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  theme_gg("Josefin Sans") +
  labs(x = "", y ="", title = "Feeling Thermometer for Democrats") +
  ggsave("dem_therm_ridges.png", type = "cairo-png", width = 10)



nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
              nonden=frcode(nonden==0 ~ "Denominational",
                            nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ggplot(., aes(x = q29_2, y = nonden)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  # scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  theme_gg("Josefin Sans") +
  labs(x = "", y ="", title = "Feeling Thermometer for Republicans") +
  ggsave("rep_therm_ridges.png", type = "cairo-png", width = 10)