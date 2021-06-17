# The code is for creating bar chart race with dynamic axis
# Data source: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?most_recent_year_desc=false

# load libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(magrittr)
library(countrycode)
library(showtext)

# loading fonts
font_add_google("Roboto", "Roboto")
showtext_auto()

# reading the data
gdp <- read.csv("C:/Users/amalasi/Documents/R/Blog post/Blog 14-animate/GDP/GDP.csv", 
                skip = 4,stringsAsFactors = F)

# removing reduncdant columns
gdp %<>% select(-c(2:4,65,66))

# converting the wide table to long
gdp_long <- gdp %>% gather(Year,GDP_dollar,c(2:61)) 

# cleaning the Year column
gdp_long$Year <- as.integer(gsub("X","",gdp_long$Year))


# data preparation
c <- gdp_long %>% mutate(Code = countrycode(gdp_long$Country.Name,
                                            "country.name","iso3c")) %>% 
  filter(!is.na(Code)) %>%
  group_by(Year) %>% 
  mutate(top_10 = rank(-GDP_dollar), GDP_bil_dol = GDP_dollar/1e9) %>% 
  #group_by(Country.Name) %>% 
  filter(top_10 <= 10) %>% ungroup()  

# setting the theme
theme_set(theme_light(base_size = 15, base_family = "Roboto"))

# creating static plot

c %>% filter(Year==1960) %>% 
  ggplot(aes(y = desc(top_10),x = GDP_bil_dol)) +
  geom_bar(stat = "identity", aes(fill = Country.Name),alpha = 0.3) +
  geom_text(aes(x=GDP_bil_dol/2,label = paste0(Country.Name,": ",round(GDP_bil_dol,1)," bn USD"), 
                hjust=0), color = "gray30") +
  #scale_x_continuous(labels = scales::comma) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust=-1.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "Roboto", size = 12),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic")
  ) + 
  labs(y="", x="GDP, bn USD") +
  ggtitle("Global GDP Ranking", subtitle = "Top 10 ranked countries for year: {frame_time}") + labs(caption = " Data: World  Bank")

# code for animating the plot 

xx <- c %>% group_by(Year) %>% 
  ggplot(aes(y = desc(top_10),x = GDP_bil_dol)) +
  geom_barh(stat = "identity", aes(fill = Country.Name),alpha = 0.3) +
  geom_text(aes(x=GDP_bil_dol/2,label = paste0(Country.Name,": ",round(GDP_bil_dol,1)," bn USD"), 
                hjust=0), color = "gray30") +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -1.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "Roboto", size = 12),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic")
  ) + 
  labs(y="", x="GDP, bn USD") +
  ggtitle("Global GDP Ranking", subtitle = "Top 10 ranked countries for year: {frame_time}") + labs(caption = " Data: World Bank")

# animating the plot
p2 <- xx + transition_time(Year) + 
  view_follow(fixed_x = c(0,NA), fixed_y = TRUE) 

# This chunk of code is for plotting the bar chart race with static axis
# xx <- c %>% group_by(Year) %>% 
#   ggplot(aes(desc(top_10),GDP_bil_dol, fill = Country.Name)) +
#   geom_bar(stat = "identity", alpha = 0.3) +
#   coord_flip() + 
#   geom_text(aes(y=ifelse(GDP_bil_dol<1000, 500,GDP_bil_dol/2),
#                 label = paste0(Country.Name,": ",round(GDP_bil_dol,1)," bn USD"), 
#                 hjust = 0),color = "gray30") +
#   theme(panel.grid = element_blank(),
#         legend.position = "none",
#         axis.title = element_text(size = 12),
#         axis.title.x = element_text(vjust = -1.5),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(family = "Roboto", size = 12),
#         plot.subtitle = element_text(size = 12),
#         plot.caption = element_text(size = 10, face = "italic")
#         ) + 
#   labs(x="", y="GDP, bn USD", caption = " Data: World Bank") +
#   ggtitle("Global GDP Ranking", subtitle = "Top 10 ranked countries for year: {frame_time}") 
# 
# p2 <- xx + transition_time(Year) 


# creating the gif
anim1 <- animate(p2,nframes = 240,fps = 10)

# saving the animation gif
magick::image_write(anim1, path="myanimation21.gif")
