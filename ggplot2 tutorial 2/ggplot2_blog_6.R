### blog post 6 ----
### Master data visualization with ggplot2: histograms, bar, and density plots
### https://www.kaggle.com/mruanova/us-gasoline-and-diesel-retail-prices-19952021


### loading packages --
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggridges)
library(patchwork)
### loading data ----
gasoline <- read.csv("C:/Users/amalasi/Documents/R/Blog post/ggplot2 tutorial 2/PET_PRI_GND_DCUS_NUS_W.csv")

### explore the dataset ====
str(gasoline)

summary(gasoline)

# converting column 1 to date type ####
gasoline$Date <- mdy(gasoline$Date)

# creating new columns
gasoline <- gasoline %>% mutate(Year = year(Date), 
                                Month = month(Date,label = TRUE), 
                                Week = week(Date))

# histograms ####
gasoline %>% filter(Year=="2020") %>% ggplot(aes(A1)) + geom_histogram()


# histogram (playing with bin)
f1 <- gasoline %>% ggplot(aes(A1)) +
  geom_histogram(fill="#808000",alpha=0.5) + #+#FFBB00 
  labs(x="Retail gasoline price ($/gallon)",y="Counts") +
  geom_text(geom = "text",label="Bins = 30",x=3.5,y=90,col="black")

f2 <- gasoline %>% ggplot(aes(A1)) +
  geom_histogram(bins=10,fill="blue",alpha=0.5) +#+#FFBB00
  labs(x="Retail gasoline price ($/gallon)",y="Counts") +
  geom_text(geom = "text",label="Bins = 10",x=3.5,y=187.5,col="black")

f3 <- gasoline %>% ggplot(aes(A1)) +
  geom_histogram(bins=50,fill="#FFBB00",alpha=0.5) + #+#FFBB00
  labs(x="Retail gasoline price ($/gallon)",y="Counts") +
  geom_text(geom = "text",label="Bins = 50",x=3.5,y=68,col="black")


f2+f1+f3



# for highlighting a column
#gasoline <- gasoline %>% mutate(Highlight = ifelse(Year==1995,"red","no"))
gasoline <- gasoline %>%
  mutate(Highlight = case_when(
    Year == "1995" ~ "red",
    Year == "2020" ~ "blue",
    TRUE ~ "No"
  )
)

gasoline %>% ggplot(aes(A1, fill = Highlight)) + 
  geom_histogram(alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("red"="red", "blue"="blue", "No"="black" ), guide = FALSE)


y_1995 <- gasoline %>% select(A1,Year,Month, Week) %>% filter(Year=="1995")
y_2020 <- gasoline %>% select(A1,Year,Month, Week) %>% filter(Year=="2020")

gasoline %>% ggplot(aes(A1)) + 
  geom_histogram(alpha = 0.5,fill="#FFBB00") +
  geom_histogram(data = subset(gasoline,Year=="1995"),fill="#808000",alpha=.5)+
  geom_histogram(data = subset(gasoline,Year=="2020"),fill="blue",alpha=.5) +
  labs(x="Retail gasoline price ($/gallon)",y="Counts")


# Year intervals
gasoline <- gasoline %>% 
  mutate(Year_chunk = case_when(
    Year <= 2010 ~ "Y1",
    Year > 2010 & Year<=2015 ~ "Y2",
    TRUE ~ "Y3"
  )
  )


a1 <- gasoline %>% ggplot(aes(A1,fill=Year_chunk)) + 
  geom_histogram(bins=50,alpha = 0.75) +
  labs(x="Retail gasoline price ($/gallon)",y="Counts")

a2 <- gasoline %>% ggplot(aes(A1,fill=Year_chunk)) + 
  geom_histogram(bins=50,alpha = 0.75,position="identity") +
  labs(x="Retail gasoline price ($/gallon)",y="Counts")

a1+a2


# Histogram with density plot

# gasoline %>% ggplot(aes(A1)) +
#   geom_histogram(binwidth = .1,fill="#375E97",alpha=0.5) + #FFBB00
#   geom_density(aes(y = .1*..count..),col="#375E97",size=1.5)

b1 <- gasoline %>% ggplot(aes(A1)) +
  geom_histogram(aes(y=..density..),fill="#375E97",alpha=0.5) + #FFBB00
  geom_density(col="#FFBB00",size=1.5) +
  labs(x="Retail gasoline price ($/gallon)",y="Density")

b2 <- gasoline %>% ggplot(aes(A1)) +
  geom_density(fill="#FFBB00",col="#FFBB00",alpha = 0.5,size=1.5) +
  labs(x="Retail gasoline price ($/gallon)",y="Density")


b1+b2


# frequency plot
gasoline %>% ggplot(aes(A1)) + 
  geom_histogram(alpha = 0.5, fill="#375E97",col="blue") +
  geom_freqpoly(col="#FFBB00",size=1) +
  labs(x="Retail gasoline price ($/gallon)",y="Counts")

# density plot
gasoline %>% ggplot(aes(A1)) + 
  geom_area(stat = "bin",alpha = 0.5, fill="#5BC8AC")


# price segregation
gasoline <- gasoline %>% 
  mutate(Price = case_when(
    A1 <= 1.5 ~ "Low",
    A1 > 1.5 & A1<=2.5 ~ "Medium",
    A1 > 2.5 & A1<=3.5 ~ "High",
    TRUE ~ "Extreme"
  )
  )


# bar plot
c1 <- gasoline %>% ggplot(aes(Year,A1,fill=Price)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#3D5B59","#B5E5CF","#FCB5AC","#B99095"))+
  labs(y="Retail gasoline price ($/gallon)",x="Year")

c2 <- gasoline %>% ggplot(aes(Year,A1,fill=Price)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#3D5B59","#B5E5CF","#FCB5AC","#B99095"))+
  labs(y="Retail gasoline price ($/gallon)",x="Year")

c3 <- gasoline %>% filter(Year=="2008") %>% ggplot(aes(Year,A1,fill=Price)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#3D5B59","#B5E5CF","#FCB5AC","#B99095"))+
  labs(y="Retail gasoline price ($/gallon)",x="Year")

c1+c2+c3



# # percentage of price segregation
#   gasoline1 <- gasoline %>% 
#     group_by(Year, .drop = FALSE) %>%
#     count(Price)
# 
#   gasoline1 <- gasoline1 %>% 
#     group_by(Year) %>% 
#     mutate(Percent = n*100/sum(n))  
# 
# 
# # bar plot
#   gasoline1 %>% ggplot(aes(Year,Percent,fill=Price)) +
#     geom_bar(stat = "identity")

# density plot ridges
gasoline %>% filter(Year=="2020") %>% ggplot(aes(A1,Month)) +
  geom_density_ridges(aes(fill=Month),alpha=0.5) +
  scale_y_discrete(limits=rev) +
  labs(x="Retail gasoline price ($/gallon)",y="Counts")


