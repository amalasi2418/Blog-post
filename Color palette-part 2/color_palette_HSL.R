# Blog post 3

library(tidyverse)
library(ggloop)
library(tidyr)
library(gtools)

# Converting RGB to HSL

# input RGB value between 0-255
RGB <- c(123, 24, 189)

############# Calculating Hue ##############
H <- 0

# calculating max and min indices
max_RGB <- which.max(RGB)
min_RGB <- which.min(RGB)

if(max_RGB == 1 & min_RGB == 3){
  H = 60*(RGB[2]-RGB[3])/(RGB[1]-RGB[3])
  print(1)
}else if(max_RGB == 1 & min_RGB == 2){
  H = 60*(6-(RGB[3]-RGB[2])/(RGB[1]-RGB[2]))
          print(2)
}else if(max_RGB == 2 & min_RGB == 1){
  H = 60*(2+(RGB[3]-RGB[1])/(RGB[2]-RGB[1]))
          print(3)
}else if(max_RGB == 2 & min_RGB == 3){
  H = 60*(2-(RGB[1]-RGB[3])/(RGB[2]-RGB[3]))
          print(4)
}else if(max_RGB == 3 & min_RGB == 2){
  H = 60*(4+(RGB[1]-RGB[2])/(RGB[3]-RGB[2]))
          print(5)
}else {
  H = 60*(4-(RGB[2]-RGB[1])/(RGB[3]-RGB[1]))
          print(6)
}

################# calculating L & S #################

# normalizing the RGB values to the scale of [0,1]
RGB <- RGB/255

# Calculating Luminance
L <- (max(RGB)+min(RGB))/2

# Calculating Saturation
S <- ifelse(L==1,0,(max(RGB)-min(RGB))/(1-(2*L-1)))

#### HLS value calculated #########
# The H vales is between [0,360] in degrees, S and L are between [0,1]
HLS_calc <- rbind(H,L,S)