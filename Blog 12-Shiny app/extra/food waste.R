library(ggplot2)
library(tidyverse)
library(readxl)
library(plotly)

food <- read_csv("Data.csv", na = "")

# removing unnecessary columns
food <- food[,-c(1,3,4,8:11,20:22)]

glimpse(food)

names(food)[3] <- "year"

supplychain <- c("Pre-Harvest","Harvest","Grading","Storage",
                "Transport","Traders","Processing","Packaging",
                "Distribution","Wholesale","Retail","Consumer",
                "Export")

# fixing the order of levels
#supplychain <- factor(supplychain, levels = supplychain)

# typo correction in fsc_location1
food$fsc_location1 <- ifelse(food$fsc_location1 == "Havest","Harvest",food$fsc_location1) 


#food_new <- food %>% filter(!(is.na(fsc_location1)))
food_new <- food %>% filter(fsc_location1 %in% supplychain)

# fixing the order of levels
food_new$fsc_location1 <-  food_new %>% factor(fsc_location1,levels=fsc_location1)

food_new %>% ggplot(aes(fsc_location1, loss_per_clean,col=fsc_location1))+
  geom_boxplot() + theme(legend.position = "none")


p <- food_new %>% ggplot(aes(year,loss_per_clean,col=fsc_location1,
                             text=row.names(food_new))) + geom_point()

ggplotly(p)

country_names <- c(unique(food_new$country),"World")

