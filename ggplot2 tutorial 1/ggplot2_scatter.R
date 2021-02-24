# blog post 4
# tutorial for using ggplot2 package for plotting.

library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggforce)


# we will be working on the data on global change in temperatures
# data source: https://www.kaggle.com/sevgisarac/temperature-change

# loading data
gtemp <- read.csv("C:/Users/amalasi/Documents/R/Blog post/ggplot2 tutorial/Environment_Temperature_change_E_All_Data_NOFLAG.csv")

# lets get an idea of the data set
str(gtemp)

# statistical summary
summary(gtemp)

# lets extract data for India
gtemp_India <- gtemp %>% filter(Area == "India")

# removing unnecessary variables
gtemp_India <- gtemp_India[,-c(1:3,5,7)]

# extracting yearly average data

gtemp_India1 <- gtemp_India %>% filter(Months == "Meteorological year")
gtemp_India1 <- gtemp_India1[,-1]
gtemp_India1 <- t(gtemp_India1)

t <- rownames_to_column(as.data.frame(gtemp_India1), var = "Year")
t <- t[-1,]
t <- strsplit(t[,1],"Y")
t <- as.numeric(unlist(t))

t <- t[!is.na(t)]
t <- cbind(t,gtemp_India1[-1,])

colnames(t) <- c("Year", "Avg_temp", "Std_dev")


gtemp_India_yr <- apply(t[ , ], 2,            # Specify own function within apply
         function(x) as.numeric(as.character(x)))
gtemp_India_yr <- as.data.frame(gtemp_India_yr)


s <- as.numeric(as.matrix(t))
 
# extracting monthly data between 1962-2019

months <- c("January","February","March","April","May","June","July","August",
            "September","October","November","December")

gtemp_India2 <- gtemp_India %>% filter(Months %in% months & Element == "Temperature change")
gtemp_India2 <- gtemp_India2[,-2]
a <- t(gtemp_India2)
a <- rownames_to_column(as.data.frame(a), var = "Year")

colnames(a) = c("Year",months)
a <- a[-1,]
b <- strsplit(a[,1],"Y")
b <- as.numeric(unlist(b))

b <- b[!is.na(b)]

b <- cbind(b,a[,-1])
colnames(b) = c("Year",months)
#c <- a[!is.na(as.numeric(unlist(strsplit(a[,1],"Y")))),]

# as.charcater doesnot work when reading from read.csv
# good reference (https://stackoverflow.com/questions/6328771/changing-values-when-converting-column-type-to-numeric)
b[,2:13] <- as.numeric(as.matrix(b[,2:13]))
gtemp_India_month <- b



### scatter plot
p1 <- gtemp_India_yr %>% ggplot(aes(Year,Avg_temp)) + 
  geom_point() 

### default smooth fit
p2 <- gtemp_India_yr %>% ggplot(aes(Year,Avg_temp)) + 
  geom_smooth(stat = "smooth", color = "blue", size = 1.5) +
  geom_point() +
  labs(x = "Years", y = "Avg. Temperature (°C)")

### linear fit
p3 <- gtemp_India_yr %>% ggplot(aes(Year,Avg_temp)) + 
  geom_smooth(method = "lm", se = FALSE,
              color = "blue", size = 1.5) +
  geom_point() +
  labs(x = "Years", y = "Avg. Temperature (°C)")

### user defined formula for fitting
p4 <- gtemp_India_yr %>% ggplot(aes(Year,Avg_temp)) + 
  geom_smooth(method = "lm", 
              formula = y~x+I(x^2)+I(x^3),
              color = "blue", size = 1.5) +
  geom_point() +
  labs(x = "Years", y = "Avg. Temperature (°C)")

p2+theme(plot.margin=unit(c(2,10,2,2),"mm"))+
  p3+theme(plot.margin=unit(c(2,10,2,2),"mm"))+p4

p5 <- gtemp_India_yr %>% ggplot(aes(Year,Avg_temp)) +
  geom_smooth(method = "lm", se = FALSE,
              color = "blue", size = 1.5) +
  geom_point(alpha = 0.5, size = 2) +
  labs(x = "Years", y = "Avg. Temperature (°C)",
       title = "Temperature rise in India",
       subtitle = "Yearly temperature fluctuations between 1961-2019",
       caption = "Data source: Kaggle.com") +
  scale_x_continuous(breaks=seq(1960,2020,5))
p5


p11 <- p1 + theme(plot.margin=unit(c(2,2,10,2),"mm"))

p11/p5

#########################
gtemp_India2[,2:60] <- as.numeric(as.matrix(gtemp_India2[,2:60]))
gtemp_India_month <- gtemp_India2

# boxplot


trial <- gtemp_India_month %>% pivot_longer(!Months,names_to = "Year", values_to = "Avg_temp")
# don't forget to convert to data frame
bb <- as.data.frame(trial[,2])
bb <- strsplit(bb[,1],"Y")
bb <- as.numeric(unlist(bb))

bb <- bb[!is.na(bb)]

gtemp_India_month1 <- cbind(bb,trial[,-2])

# avoid getting factors from arranging in alphabetical order, we define levels
gtemp_India_month1[,2] <- factor(gtemp_India_month1[,2],levels = months)


# base plot
b0 <- gtemp_India_month1 %>% ggplot(aes(Months,Avg_temp))



# default boxplot for monthly data-
b1 <- gtemp_India_month1 %>% ggplot(aes(Months,Avg_temp)) + 
  geom_boxplot() + theme(legend.position = "none")


# informative: add jitter to boxplot to tell data distribution - messy
b2 <- b0 +geom_boxplot(aes(col = Months, fill = Months), alpha = 0.25)  
  

b21 <- b2 + theme(legend.position = "none")

# adding theme to control axis, legend, caption
b3 <- b2 + geom_jitter(aes(col = Months), alpha = 0.5, width = 0.25) + 
  geom_sina(aes(col = Months)) +
  labs(x = "Months", y = "Avg. Temperature (°C)",
  title = "Temperature rise in India",
  subtitle = "Monthly temperature fluctuations between 1961-2019",
  caption = "Data source: Kaggle.com") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# combining the plots
b1+b21+b3


# make it sassy with violin plots
b4 <- gtemp_India_month1 %>% 
  ggplot(aes(Months,Avg_temp)) + 
  geom_violin(aes(col = Months, fill = Months), alpha = 0.25)+
  geom_sina(aes(col = Months), alpha = 0.5) +
  labs(x = "Months", y = "Avg. Temperature (°C)",
         title = "Temperature rise in India",
         subtitle = "Monthly temperature fluctuations between 1961-2019",
         caption = "Data source: Kaggle.com") +
  coord_flip() +
  scale_x_discrete(limits=rev)

b5 <- gtemp_India_month1 %>% 
  ggplot(aes(Months,Avg_temp)) + 
  geom_violin(aes(col = Months, fill = Months), alpha = 0.25)+
  labs(x = "Months", y = "Avg. Temperature (°C)",
       title = "Temperature rise in India",
       subtitle = "Monthly temperature fluctuations between 1961-2019",
       caption = "Data source: Kaggle.com") +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  geom_boxplot(color = "gray20", width = 0.15, coef = 1.5)



b11 <- b1+coord_flip() +
  scale_x_discrete(limits=rev) 


b31 <- b2 + labs(x = "Months", y = "Avg. Temperature (°C)",
                 title = "Temperature rise in India",
                 subtitle = "Monthly temperature fluctuations between 1961-2019",
                 caption = "Data source: Kaggle.com") + 
  coord_flip() +
  scale_x_discrete(limits=rev)

b11+theme(plot.margin=unit(c(2,15,2,2),"mm"))+b31

b11+theme(plot.margin=unit(c(2,15,2,2),"mm"))+b5


