library(socsci)
library(tidytext)
library(ggrepel)

names <- read_csv("D://yp_scrapes/full_names.csv")

names <- names %>% 
  distinct(id, .keep_all = TRUE)

tidy <- names %>% 
  unnest_tokens(word, name) %>% 
  anti_join(stop_words, by = c("word" = "word"))


count <- tidy %>% 
  ct(word) %>% 
  mutate(pct = n/1078) %>% 
  arrange(-pct)

count %>%
  filter(word != "church") %>% 
  top_n(25, pct) %>% 
  ggplot(., aes(x = reorder(word, pct), y = pct)) +
  geom_col(alpha = .7) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  theme_gg("Josefin Sans") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.1, color="black")) +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "", y = "", title = "Most Frequently Used Words in Non-Denominational Church Names", caption = "Data: ARC Website") +
  ggsave("D://nd/names_places/first_frequency.png")


### Church by City ### 

total <- names %>% 
  ct(city) %>% 
  select(city, n)

church <- tidy %>% 
  group_by(city) %>% 
  ct(word) %>% 
  filter(word == "church") %>% 
  select(city, church_n = n)

pct <- left_join(total, church) %>% 
  mutate(pct = church_n/n)

pct %>% 
  ggplot(., aes(x = reorder(city, pct), y = pct)) +
  geom_col() +
  theme_gg("Josefin Sans") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "", title = "Use of 'Church' in Name by City", caption = "Data: ARC Website") +
  ggsave("D://nd/names_places/church_by_city.png", type = "cairo-png")

## Scatter ###

prri <- read_csv("D://metro_nones.csv")

scatter <- left_join(pct, prri) %>% 
  mutate(nones = nones/100)

scatter %>% 
  ggplot(., aes(x = pct, y = nones)) +
  geom_point() +
  geom_smooth(method = lm, linetype = "twodash") +
  theme_gg("Josefin Sans") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  geom_text_repel(data = scatter, aes(x = pct,y = nones, label=city), size = 4.5, family = "font") +
  theme(plot.title = element_text(size = 14)) +
  labs(x = "Percent with 'Church' in Name", y = "Percent Unaffiliated in the Metro Area", title = "Relationship Between the Nones and Likelihood of Including 'Church' in Name", caption = "Data: ARC Website +\n2018 PRRI Values Atlas") +
  ggsave("D://nd/names_places/nones_church_scatter.png", type = "cairo-png", width = 8)


## City Grid #### 

count <- tidy %>% 
  group_by(city) %>% 
  ct(word)

count1 <- tidy %>% 
  ct(city) %>% 
  select(total = n, city, -pct) 

final <- left_join(count, count1) %>% 
  mutate(pct = n/total)


final <- final %>%
  filter(word != "church") %>% 
  filter(total >= 50) %>% 
  top_n(5, pct)

final %>%
  filter(word != "church") %>% 
  top_n(5, pct) %>% 
  ggplot(., aes(x = reorder(word, pct), y = pct)) +
  geom_col(alpha = .7) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  theme_gg("Josefin Sans") +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "", y = "", title = "Most Frequently Used Words in Non-Denominational Church Names", caption = "Data: ARC Website") +
  ggsave("E://nd_names_by_city.png")


fun <- function(df, name){
  
  
  filename <- paste('D://nd/names/', name, '.png', sep = '')
  
  df %>%
    filter(word != "church") %>% 
    filter(city == name) %>% 
    top_n(5, pct) %>% 
    head(5) %>% 
    ggplot(., aes(x = reorder(word, pct), y = pct)) +
    geom_col(alpha = .7) +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    theme_gg("Josefin Sans") +
    theme(plot.title = element_text(size = 54)) +
    theme(legend.text = element_text(size = 54)) +
    theme(axis.text.x = element_text(size = 44)) +
    theme(axis.text.y = element_text(size = 44)) +
    labs(x = "", y = "", title = glue("{name}"), caption = "") +
    ggsave(filename, type = "cairo-png", width = 10, height = 10)
  
  
}

final %>% fun("NYC")
final %>% fun("Atlanta")
final %>% fun("Chicago")
final %>% fun("Baltimore")
final %>% fun("Dallas")
final %>% fun("Houston")
final %>% fun("Miami")
final %>% fun("Tampa")
final %>% fun("Minneapolis")
final %>% fun("San Fran")
final %>% fun("Orlando")
final %>% fun("San Diego")
final %>% fun("Detroit")
final %>% fun("Charlotte")
final %>% fun("San Antonio")
final %>% fun("St. Louis")


### This is to make the name grid - don't run this. 

library(png)
library(grid)
library(gridExtra)

setwd("D://nd/names")

plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})


ggsave("plots_combined.png",width=11.5, height=8, 
       grid.arrange(grobs = plots, ncol=4, nrow =4,  as.table = TRUE, top = textGrob("Non-Denominational Church Names", gp=gpar(fontsize = 15, font = 8))))



