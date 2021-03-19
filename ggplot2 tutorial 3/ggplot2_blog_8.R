library(ggplot2)
library(tidyverse)

# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
# https://www.datanovia.com/en/blog/how-to-create-a-pie-chart-in-r-using-ggplot2/
# https://coolbutuseless.github.io/2018/11/13/ggthreed-3d-pie-charts/

cereal <- read_csv("C:/Users/amalasi/Documents/R/Blog post/ggplot2 tutorial 3/cereal.csv")

summary(cereal)

str(cereal)

# create table for manufacturer variable
manufact <- count(cereal,mfr)

# calculate the percentage share of manufacturer
manufact <- manufact %>% mutate(perc=round(n*100/sum(n),1))

# calculating the cumulative for positioning the text 
manufact <- manufact %>% 
  arrange(desc(mfr)) %>% 
  mutate(y_pos = cumsum(perc)-0.5*perc)

# pie chart 
manufact %>% ggplot(aes(x="",perc, fill=mfr)) + 
  geom_bar(width=1,stat="identity",color="white",alpha=.5) + 
  coord_polar("y", start=0)+
  geom_text(aes(y = y_pos, label = paste0(perc,"%")), color = "black")+
  scale_fill_manual(values = rainbow(7)) +
  theme_void() 

# manufact %>% ggplot(aes(x="",perc, fill=mfr)) + 
#   geom_bar(stat="identity") + 
#   coord_polar("x", start=0,direction = -1)

# radial bar plot
manufact %>% ggplot(aes(x=fct_reorder(mfr, perc, .desc = FALSE),n)) + 
  geom_bar(stat="identity",fill =rainbow(7),alpha=0.5) + 
  coord_polar("y", start=0,direction = 1)+
  ylim(0,25) + xlab("Different manufacterers") +ylab("# of different cereals")

manufact %>% ggplot(aes(mfr,n, fill=mfr)) + 
  geom_bar(stat="identity") + 
  coord_polar("x", start=0,direction = -1)+
  xlab("Different manufacterers") +
  ylab("# of different cereals")

# donut
manufact %>% ggplot(aes(x=2,perc, fill=mfr)) + 
  geom_bar(stat="identity",color="white", alpha =.5) + 
  coord_polar(theta = "y", start=0)+
  geom_text(aes(y = y_pos, label = paste0(perc,"%")), color = "black")+
  scale_fill_manual(values = rainbow(7)) +
  theme_void() +
  xlim(0.5, 2.5)


# installing ggradar package
#devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)


library(ggradar)

# lets compare the mineral composition of different Bran cereals
cereal_Bran_low <- cereal  %>% 
  filter(str_detect(name, "Bran") & calories > 80 & calories <120) %>% 
  select(1,5:6,8,10)

ggradar(
  cereal_Bran_low, 
  #aes(x=name),
  #resacle=TRUE,
  values.radar = c("0", "5","10"),
  grid.min = 0, grid.mid = 5, grid.max = 10,
  # Polygons
  group.line.width = 1.5, 
  group.point.size = 3,
 # group.colours = rainbow(6),
  legend.position = "bottom",
 legend.text.size = 10
)


