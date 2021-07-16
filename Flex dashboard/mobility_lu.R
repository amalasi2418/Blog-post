library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

mobility_LU <- read_csv("2020_LU_Region_Mobility_Report.csv", na = c("-","","NA"))
covid_LU <- read_excel("datapublic-covid19.xlsx",na = c("-"," ","NA"))
vaccine_LU <- read_excel("vaccination-covid19.xlsx")

# removing redundant data
mobility_LU <- mobility_LU[,-(1:8)]

# column name
colnames(mobility_LU) <- c("Date", "Retail & Recreation","Grocery & Pharmacy","Parks","Transit stations","Workplaces","Residential places")
colnames(vaccine_LU) <- c("Date","Vac_dose1","Vac_dose2","Total_dose")

# standardizing date format
# the date is chr format and should be converted to Date format
covid_LU$Date <- as.Date(covid_LU$Date, format = "%Y-%m-%d")
vaccine_LU$Date <- as.Date(vaccine_LU$Date, format = "%Y-%m-%d")

# merging tables
LU_1 <- full_join(mobility_LU,covid_LU,all=TRUE)
LU <- full_join(LU_1,vaccine_LU,all=TRUE)

LU %>% ggplot() + geom_line(aes(Date,`Retail & Recreation`,col="green")) +
  geom_line(aes(Date,Workplaces,col="blue")) +
  geom_line(aes(Date,Parks,col="red"))

write.csv(LU,"Covid_LU.csv")                  
