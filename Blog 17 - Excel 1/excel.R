

setwd("C:/Users/amalasi/Documents/R/Blog post/Blog 17 - Excel 1")

# load packages
library(xlsx)
library(dplyr)
library(tidyverse)


# reading individual files
# fuel <- read.xlsx2("global-fuel-vs-gdp.xlsx",sheetIndex = 1)
# life_exp <- read.xlsx2("global-life-expectancy.xlsx",sheetIndex = 1)
# water <- read.xlsx2("global-water-share.xlsx",sheetIndex = 1)


# automating file reading

# create list of xlsx files in the working directory
# list the files in the working folder
data.files <- list.files(pattern = "*.xlsx")

# read the files
files <- lapply(data.files, function(x) read.xlsx2(x, sheetIndex = 1))

# load workbooks to separate objects
for (i in 1:length(data.files)){    
  # loading the workbook or excel file
  wbook <- loadWorkbook(data.files[i])   
  
  #extracting sheets from the individual workbooks
  sheet <- getSheets(wbook)                      
  
  for (j in 1:length(sheet)){ 
    # assigning names to individual sheets of the workbooks
    # naming convention: i=file index, j=sheet number (31 means -> file 3 sheet 1)
    assign(paste("global.", i,j, sep = ""), 
           read.xlsx2(data.files[i], sheetIndex=j, 
                      as.data.frame=TRUE, header=TRUE))
    
  }
}



# merging all the files into a single workbook


# adding files manually is cumbersome if working with lot of files
# write.xlsx(global.11, file="global-data.xlsx", sheetName="Sheet1", 
#            col.names=TRUE, row.names=TRUE, append=FALSE)
 
# write.xlsx(global.21, file="global-data.xlsx", sheetName="Sheet2", 
#            col.names=TRUE, row.names=TRUE, append=TRUE)

# write.xlsx(global.31, file="global-data.xlsx", sheetName="Sheet3", 
#            col.names=TRUE, row.names=TRUE, append=TRUE)



for (i in 1:length(data.files)) {
      if(i==1)
      write.xlsx(files[[i]], file="global-data.xlsx", 
                 sheetName = paste("global",i))
      else 
      write.xlsx(files[[i]], file="global-data.xlsx", 
                 sheetName = paste("global",i), append=TRUE)
}



# splitting excel sheets

split_file <- loadWorkbook("global-data.xlsx")
split_sheet <- getSheets(split_file)

for (j in 1:length(split_sheet)){ 
 assign(paste("global_split_",j, sep = ""), 
         read.xlsx2("global-data.xlsx", sheetIndex=j, 
                    as.data.frame=TRUE, header=TRUE))
}


# filling in the Continent column
# assign empty columns NA value
# then use fill() function to fill in the Continent data


global_split_1 <-mutate_all(global_split_1, list(~na_if(.,"")))


global_split_1 <- global_split_1 %>% group_by(Entity,Code) %>% 
  fill(Continent, .direction = "downup") %>% 
  ungroup()




# combine the sheets into one single sheet

# doing manually
#global_final <- merge(x = global_split_1[,-1], y = global_split_2[,-1], by = c("Entity","Code","Year"), all = TRUE)
#global_final <- merge(x = global_final, y = global_split_3[,-1], by = c("Entity","Code","Year"), all = TRUE)

#############
# Automating

# removing index column
global_split_1<- global_split_1[,-1]
global_split_2<- global_split_2[,-1]
global_split_3<- global_split_3[,-1]


# create list of data frames with similar pattern
local_list <- mget(x = ls(pattern = "global_split_[1-3]*"))


for (i in 2:length(local_list)) {
  if(i==2){
    global_final <- merge(x = local_list[[paste0("global_split_",i-1)]], 
                           y = local_list[[paste0("global_split_",i)]], 
                           by = c("Entity","Code","Year"), all = TRUE)
  }else
    global_final <- merge(x = global_final, 
                           y = local_list[[paste0("global_split_",i)]], 
                           by = c("Entity","Code","Year"), all = TRUE)
}

#############

# create folders

conti <- unique(global_final$Continent)

for (i in 1:n_distinct(global_final$Continent)) {
  
  dir.create(conti[i])
}


# creating separate files for each country and placing them 
# in the continent folder they belong to

# Create a numeric id for countries 
global_final$Entity_id = as.numeric(as.factor(global_final$Entity))


for (i in 1:n_distinct(global_final$Entity_id)){
  temp <- global_final %>% filter(Entity_id == i) %>% 
    select(-c(2,7,12))
  
  ## Create a new workbook
  w_book <- createWorkbook(type = "xlsx")
  
  ## Add some worksheets
  new_sheet <- createSheet(w_book, sheetName="Data")
  
  addDataFrame(temp, new_sheet, startRow=1, startColumn=1)
  
  ## Save workbook
  id <- max(which(grepl(i, global_final$Entity_id)))
  saveWorkbook(w_book, paste0(global_final$Continent[id],"/",global_final$Entity[id],".xlsx"))
}

