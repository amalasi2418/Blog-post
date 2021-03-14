
library(ggplot2)
library(tidyverse)

n <- 1000
col <- 25
row <- n/col

pie <- read.csv("pi.csv")

tab <- table((pie[,1]))
tab <- count(pie,from)

tab %>% ggplot(aes(from,n,fill=rainbow(10),alpha=0.6))+
  geom_bar(stat="identity",)+
  coord_polar(theta = "x")

coord_x <- rep.int(0:24,40)
#coord_y <- rep.int(10,25)
series <- 0:999
coord_y <- as.integer(series%/%col)
pie <- cbind(pie,coord_x,coord_y)

pie %>% ggplot(aes(x=coord_x,y=coord_y, color = factor(pie[,1]))) +
  geom_text(aes(label=pie[,1], color=factor(pie[,1])), size=3) +
  coord_fixed() + 
  theme_void() + 
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))


pie %>% mutate(even = from %% 2) %>% 
  ggplot(aes(x=coord_x,y=coord_y, color = factor(pie[,1]))) +
  geom_point(aes(shape=factor(even)), alpha=0.9, size=3) +  #change shape to color for odd/even
  coord_fixed() + 
  theme_void() + 
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  scale_shape_manual(values=c(19,21), guide="none")


pie %>% ggplot(aes(col = rainbow(n))) +
  geom_segment(aes(x=pie[,1],xend=pie[,2],y=0,yend=1), size = 0.5) +
  theme_void() + 
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  scale_shape_manual(values=c(19,21), guide="none")


pie %>% ggplot(aes(col = rainbow(n))) +
  geom_segment(aes(x=pie[,1],xend=pie[,2],y=0,yend=1), size = 0.5) +
  theme_void() + 
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  scale_shape_manual(values=c(19,21), guide="none") +
  coord_polar(theta = "x")


ggsave(
  filename = "silk24.jpeg",
  device = jpeg(),
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  units = "in",
  dpi = 600,
  limitsize = TRUE
  #bg = "transparent"
)

dev.off()

