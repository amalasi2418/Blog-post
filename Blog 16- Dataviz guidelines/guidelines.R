library(ggplot2)
library(tidyverse)
library(patchwork)
library(scales)
library(magrittr)


# read data file
age <- read.csv("life-expectancy.csv")


# rename columns
age <- age %>% rename(Country = Entity)


# countries of G8 summit
G8 <- c("Canada","France","Germany","Italy",
        "Japan","Russia","United Kingdom","United States")

# data for G8 summit members
G8_2019 <- age %>% 
  filter(Country %in% G8, Year == 2019) %>% 
  select(Country,Life.expectancy)

# top 10 countries with high life expectancy
top_10 <- age %>% filter(Year == 2019) %>% arrange(desc(Life.expectancy)) %>%
  select(Country,Life.expectancy) 

# bottom 10 countries with lowest life expectancy
bot_10 <- age %>% filter(Year == 2019) %>% arrange(Life.expectancy) %>%
  select(Country,Life.expectancy) 

# creating dataframe of 20 rows
age_extreme <- rbind(top_10[1:10,],bot_10[1:10,])

# creating dataframe of 10 rows
age_extreme_1 <- rbind(top_10[1:5,],bot_10[1:5,])


# guideline 1
p1 <- age_extreme_1 %>% ggplot(aes(Country,Life.expectancy)) + 
  geom_bar(stat = "identity") + ylab("Life expectancy") +
  scale_y_continuous(limits = c(0,100))


p2 <- age_extreme_1 %>% ggplot(aes(Country,Life.expectancy)) + 
  geom_point(col="black",size=3) + theme_minimal() +
  scale_y_continuous(limits = c(0,100)) + ylab("Life expectancy")


p1+p2

ggsave("fig1.jpg", plot = last_plot())


# guideline 2
p3 <- age_extreme_1 %>% ggplot(aes(Country,Life.expectancy)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1)) +
  ylab("Life expectancy") + scale_y_continuous(limits = c(0,100)) 
  
 

p4 <- age_extreme_1 %>% ggplot(aes(Country,Life.expectancy)) + 
  geom_bar(stat = "identity") +
  ylab("Life expectancy") +
  coord_flip() +
  scale_y_continuous(limits = c(0,100)) + theme_minimal()

p3+p4

ggsave("fig2.jpg", plot = last_plot())


# guideline 3

p5 <- age_extreme %>%
  ggplot(aes(Country,Life.expectancy)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Life expectancy") +
  scale_y_continuous(limits = c(0,100)) + theme_minimal()

p6 <- age_extreme %>%
  ggplot(aes(Country,Life.expectancy)) + 
  geom_point(col="black") +
  coord_flip() + ylab("Life expectancy") +
  scale_y_continuous(limits = c(0,100)) + theme_minimal()


p7 <- age_extreme %>%
  ggplot(aes(fct_reorder(Country,Life.expectancy),Life.expectancy)) + 
  geom_bar(stat="identity") +
  coord_flip() + 
  ylab("Life expectancy") +
  xlab("Country") +
  scale_y_continuous(limits = c(0,100)) + 
  theme_minimal()

p8 <- age_extreme %>%
  ggplot(aes(Life.expectancy,fct_reorder(Country,Life.expectancy))) + 
  geom_point(col="black") +
  scale_x_continuous(limits = c(0,100)) + xlab("Life expectancy") + 
  ylab("Country") +
  theme_minimal()

(p5+p7)
ggsave("fig3_1.jpg", plot = last_plot())

(p6+p8)
ggsave("fig3_2.jpg", plot = last_plot())


# guideline 4

UK_age <- age %>% filter(Country=="United Kingdom") %>% 
  select(Year,Life.expectancy)

arrow <- data.frame(x1=c(1907,1925),
                    y1=c(75,25),
                    x2=c(1916,1919),
                    y2=c(75,25))

p9 <- UK_age %>% ggplot(aes(Year,Life.expectancy)) +
  geom_line(col="black") +
  xlim(xmin=1850,xmax=2019) +
  ylim(ymin=0,ymax=100) +
  annotate(geom = "rect", xmin = 1914, xmax = 1918, ymin = -Inf, ymax = Inf,
           fill = "palegreen", alpha = 0.5) +
  annotate(geom = "rect", xmin = 1918, xmax = 1920, ymin = -Inf, ymax = Inf,
           fill = "orange", alpha = 0.5) +
  ylab("Life expectancy") +
  theme_minimal() +
  annotate("text", x = 1890, y = 75, label = "World War I",col="black") +
  annotate("text", x = 1940, y = 25, label = "Spanish Flu",col="black") +
  geom_segment(data=arrow, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.08, "inch")),color="black",size=0.5)
   
ggsave("fig4.jpg", plot = last_plot())


# reading GDP data
GDP_age <- read.csv("life-expectancy-vs-gdp-per-capita.csv")

# filtering data
GDP_age_2018 <- GDP_age %>% 
  filter(Year==2018) %>% 
  select(Entity,Life.expectancy,GDP.per.capita)


# remove NA
GDP_age_2018 %<>% na.omit()


# creating label
label <- GDP_age_2018 %>% filter(Life.expectancy == min(Life.expectancy)| 
                                   GDP.per.capita == max(GDP.per.capita)) 


GDP_age_2018 %>% ggplot(aes(GDP.per.capita,Life.expectancy)) + 
  geom_point(alpha=0.5) + theme_minimal() +
  ylim(ymin=0,ymax=100) +
  ylab("Life expectancy") +
  xlab("GDP per capita") +
  geom_point(data=label, aes(GDP.per.capita,Life.expectancy), col = "red") +
  geom_text(data = label, aes(label = Entity),color = "blue",
            vjust = "inward", hjust = "inward")

ggsave("fig4-1.jpg", plot = last_plot())


# guideline 5
G8_age <- age %>% filter(Country %in% G8) 

G8_age %>% ggplot(aes(Year,Life.expectancy)) +
  geom_line(col="Black") + facet_wrap(~Country,nrow = 2) +
  ylim(ymin=0,ymax=100) + ylab("Life expectancy") +theme_minimal() +coord_flip()


ggsave("fig5.jpg", plot = last_plot())


# guideline 6

GDP_age_2018 %>% ggplot(aes(GDP.per.capita,Life.expectancy)) + 
  geom_point() + theme_minimal() +
  ylim(ymin=0,ymax=100) +
  ylab("Life expectancy") +
  xlab("GDP per capita") +
  scale_x_log10(labels = dollar) +
  geom_smooth(method='lm', formula= y~x,col="red") 
  
ggsave("fig6.jpg", plot = last_plot())
###########################

 
  
