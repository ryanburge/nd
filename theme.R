library(showtext)
library(ggthemes)
library(ggsci)
library(ggthemes)

theme_gg <- function(fff, base_size = 18, base_family = "font", legend = FALSE){
  
  font_add_google(fff, "font")
  showtext_auto()
  showtext_opts(dpi = 300)
  
  
  if(legend == TRUE){
    theme_hc() +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(text=element_text(size=18, family="font"))
    
  }
  
  else{
    
    theme_hc() +
      theme(legend.position = "none") +
      theme(legend.title = element_blank()) +
      theme(text=element_text(size=18, family="font"))
  }
  
}



## Bar Labels ####
lab_bar <- function(type, pos, sz, top = TRUE){
  
  type <- enquo(type)
  
  if(top == FALSE) {
    
    geom_text(aes(y = pos, label = paste0(!! type*100, '%')), position = position_dodge(width = .9), size = sz, family = "font")   
    
  }
  
  else{
    
    geom_text(aes(y = !! type + pos, label = paste0(!! type*100, '%')), position = position_dodge(width = .9), size = sz, family = "font")
    
  } 
  
}

## Error Bars ####
error_bar <- function(wd){
  
  if(missing(wd)){
    
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) 
  } else{
    
    geom_errorbar(aes(ymin=lower, ymax=upper), width=wd, position=position_dodge(.9)) 
  }
}


## Ribbons ####
ribbons <- function(grp){
  
  if(missing(grp)){
    
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .4, show.legend = FALSE) 
    
  } else{
    
    geom_ribbon(aes(ymin=lower, ymax=upper, color = grp, fill = grp), alpha = .4, show.legend = FALSE) 
    
  }
}

## Percents ####
x_pct <- function(){
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 5L))
  
}

y_pct <- function(){
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
  
}


## Annotates #### 

add_text <- function(x, y, sz, word){
  
  if(missing(sz)){
    
    annotate("text", x = x, y = y, label = word, size = 6, family = "font") 
  }else{
    
    annotate("text", x = x, y = y, label = word, size = sz, family = "font") 
  }
  
}


## Colors ### 

pid3_fill <- function(){
  
  scale_fill_manual(values = c("dodgerblue3", "azure3", "firebrick3"))
  
}


pid3_color <- function(){
  
  scale_color_manual(values = c("dodgerblue3", "azure3", "firebrick3"))
  
}

fill4_1 <- function(){
  
  scale_fill_manual(values = c("#2B5B6C", "#C7CFAC", "#FCFAF1", "#E34F33"))
  
}

color4_1 <- function(){
  
  scale_color_manual(values = c("#2B5B6C", "#C7CFAC", "#FCFAF1", "#E34F33"))
  
}


fill4_2 <- function(){
  
  scale_fill_manual(values = c("#337A58", "#383E4E", "#F9F6E5", "#FCCAAC"))
  
}

color4_2 <- function(){
  
  scale_color_manual(values = c("#337A58", "#383E4E", "#F9F6E5", "#FCCAAC"))
  
}


fill4_3 <- function(){
  
  scale_fill_manual(values = c("#254A5D", "#C7518F", "#7BCCD4", "#51629E"))
  
}

color4_3 <- function(){
  
  scale_color_manual(values = c("#254A5D", "#C7518F", "#7BCCD4", "#51629E"))
  
}



fill5_1 <- function(){
  
  scale_fill_manual(values = c("#612C69", "#459ED5", "#FD823E", "#FCCF61", "#E1EFCE"))
  
}

color5_1 <- function(){
  
  scale_color_manual(values = c("#612C69", "#459ED5", "#FD823E", "#FCCF61", "#E1EFCE"))
  
}

fill5_2 <- function(){
  
  scale_fill_manual(values = c("#99dedf", "#e45865", "#fcd06b", "#f7f6ee", "#ffd4c4"))
  
}

color5_2 <- function(){
  
  scale_color_manual(values = c("#99dedf", "#e45865", "#fcd06b", "#f7f6ee", "#ffd4c4"))
  
}


fill5_3 <- function(){
  
  scale_fill_manual(values = c("#1D366D", "#2E6095", "#F8F1E0", "#B14145", "#64C2E4"))
  
}

color5_3 <- function(){
  
  scale_color_manual(values = c("#1D366D", "#2E6095", "#F8F1E0", "#B14145", "#64C2E4"))
  
}


fill5_4 <- function(){
  
  scale_fill_manual(values = c("#104253", "#e66453", "#cec238", "#f4efdf", "#91a5ac"))
  
}

color5_4 <- function(){
  
  scale_color_manual(values = c("#104253", "#e66453", "#cec238", "#f4efdf", "#91a5ac"))
  
}


fill5_5 <- function(){
  
  scale_fill_manual(values = c("#344d77", "#c55a82", "#ffd34b", "#ff7241", "#62dcda"))
  
}

color5_5 <- function(){
  
  scale_color_manual(values = c("#344d77", "#c55a82", "#ffd34b", "#ff7241", "#62dcda"))
  
}

fill5_scale <- function(){
  
  scale_fill_manual(values = c("#EE4035", "#F37736", "#FDF498", "#7BC043", "#0492CF")) 
  
}

color5_scale <- function(){
  
  scale_color_manual(values = c("#EE4035", "#F37736", "#FDF498", "#7BC043", "#0492CF")) 
  
}

## GSS Reltrad ### 


gss_reltrad <- function(df, var){
  
  var <- enquo(var)
  
  df %>% 
    ungroup(!! var) %>% 
    mutate(reltrad = frcode(!! var == 1 ~ "Evangelical",
                            !! var == 2 ~ "Mainline",
                            !! var == 3 ~ "Black Prot.",
                            !! var == 4 ~ "Catholic",
                            !! var == 5 ~ "Jewish",
                            !! var == 6 ~ "Other Faith",
                            !! var == 7 ~ "No Religion", 
                            TRUE ~ "Unclassified")) 
}

## GSS pid3 ####

gss_pid3 <- function(df, var){
  
  var <- enquo(var)
  
  df %>% 
    mutate(pid3 = frcode(!! var == 0 | !! var == 1 | !! var == 2 ~ "Democrat", 
                         !! var == 3 ~ "Independent",
                         !! var == 4 | !! var == 5 | !! var == 6 ~ "Republican"))
}

## CCES Race ####

cces_race <- function(df, var){
  
  var <- enquo(var)
  
  df %>% 
    mutate(race = frcode(!! var == 1 ~ "White",
                         !! var == 2 ~ "Black",
                         !! var == 3 ~ "Hispanic",
                         !! var == 4 ~ "Asian",
                         TRUE ~ "All Others"))
  
}


## CCES Attend ####

cces_attend <- function(df, var){
  
  var <- enquo(var)
  
  df %>% 
    mutate(att = car::recode(!! var, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
    mutate(att = frcode(att == 1 ~ "Never",
                        att == 2 ~ "Seldom",
                        att == 3 ~ "Yearly",
                        att == 4 ~ "Monthly",
                        att == 5 ~ "Weekly",
                        att == 6 ~ "Weekly+")) 
  
}

## Smoothed Lines ####
smooth <- function(){
  
  geom_smooth(se = FALSE, linetype = "twodash")
  
}

