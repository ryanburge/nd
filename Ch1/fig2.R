
names <- read_csv("D://yp_scrapes/full_names.csv")

names <- names %>% 
  distinct(id, .keep_all = TRUE)

tidy <- names %>% 
  unnest_tokens(word, name) %>% 
  anti_join(stop_words, by = c("word" = "word"))


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


ggsave("D://nd/Ch1/images/fig2.png",width=11.5, height=8, 
       grid.arrange(grobs = plots, ncol=4, nrow =4,  as.table = TRUE, top = textGrob("", gp=gpar(fontsize = 15, font = 8))))


rm(plots)
