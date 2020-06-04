library(fst)
library(socsci)
library(car)
library(reshape2)
library(ggalluvial)

cces <- read.fst("C://cces_panel.fst")


clean <- cces %>% 
  mutate(prot10 = frcode(religpew_protestant_10 == 1 ~ "Baptist",
                         religpew_protestant_10 == 2 ~ "Methodist",
                         religpew_protestant_10 == 3 ~ "Nondenom.",
                         religpew_protestant_10 == 4 ~ "Lutheran",
                         religpew_protestant_10 == 5 ~ "Presbyterian",
                         religpew_protestant_10 == 6 | religpew_protestant_10 == 10 ~ "Pentecostal",
                         religpew_protestant_10 == 7 | religpew_protestant_10 == 8 | religpew_protestant_10 == 9 ~ "ECUSA/DoC/UCC", 
                         religpew_10 == 9 | religpew_10 == 10 | religpew_10 == 11 ~ "Nones",
                         religpew_10 == 2 ~ "Catholic",
                         TRUE ~ "All Others")) %>% 
  mutate(prot12 = frcode(religpew_protestant_12 == 1 ~ "Baptist",
                         religpew_protestant_12 == 2 ~ "Methodist",
                         religpew_protestant_12 == 3 ~ "Nondenom.",
                         religpew_protestant_12 == 4 ~ "Lutheran",
                         religpew_protestant_12 == 5 ~ "Presbyterian",
                         religpew_protestant_12 == 6 | religpew_protestant_12 == 10 ~ "Pentecostal",
                         religpew_protestant_12 == 7 | religpew_protestant_12 == 8 | religpew_protestant_12 == 9 ~ "ECUSA/DoC/UCC",
                         religpew_12 == 9 | religpew_12 == 10 | religpew_12 == 11 ~ "Nones",
                         religpew_12 == 2 ~ "Catholic",
                         TRUE ~ "All Others")) %>% 
  mutate(prot14 = frcode(religpew_protestant_14 == 1 ~ "Baptist",
                         religpew_protestant_14 == 2 ~ "Methodist",
                         religpew_protestant_14 == 3 ~ "Nondenom.",
                         religpew_protestant_14 == 4 ~ "Lutheran",
                         religpew_protestant_14 == 5 ~ "Presbyterian",
                         religpew_protestant_14 == 6 | religpew_protestant_14 == 10 ~ "Pentecostal",
                         religpew_protestant_14 == 7 | religpew_protestant_14 == 8 | religpew_protestant_14 == 9 ~ "ECUSA/DoC/UCC",
                         religpew_14 == 9 | religpew_14 == 10 | religpew_14 == 11 ~ "Nones",
                         religpew_14 == 2 ~ "Catholic",
                         TRUE ~ "All Others"))  



small <- clean %>% 
  select(caseid, prot10, prot12, prot14)

test <- melt(small, id = c("caseid")) %>% arrange(caseid) 

test <- test %>% 
  mutate(year = frcode(variable == "prot10" ~ "2010",
                       variable == "prot12" ~ "2012",
                       variable == "prot14" ~ "2014"))

test %>% 
  ggplot(., aes(x = year, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
  geom_stratum() +
  theme_gg("Josefin Sans") +
  coord_flip() +
  geom_label(fill = "white", stat = "stratum", size = 4, colour = "black", family = "font") +
  scale_fill_tableau() +
  scale_color_tableau() +
  theme(plot.title = element_text(size = 20)) +
  labs(x = "Year", y = "Number of Respondents", title = "Religious Shifts Over Time", caption = "") +
  ggsave("D://nd/Ch0/images/fig2.png", width = 10) 