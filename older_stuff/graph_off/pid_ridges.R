
nd %>% mutate(nonden=car::recode(q3, "1=1; 2:17=0"),
              nonden=frcode(nonden==0 ~ "Denominational",
                            nonden==1 ~ "Non-Denominational")) %>% 
  group_by(nonden) %>% 
  ggplot(., aes(x = q29_1, y = nonden)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  theme_gg("Josefin Sans") +
  scale_x_continuous(limits = c(-.15,7), breaks = c(1,2,3,4,5,6,7), labels = c("Strong\nDemocrat", "Democrat", "Ind., but\nLean Democrat", "Independent", "Ind., but\nLean Republican", "Republican", "Strong\nRepublican")) +
  labs(x = "", y ="", title = "Distribution of Party ID") +
  ggsave("pid_ridges.png", type = "cairo-png", width = 10)
