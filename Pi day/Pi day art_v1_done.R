library(tidyverse)
library(ggplot2)
pie <- read.csv('pi.csv')

n <- 1000 # number of points in spiral
k <- 6.5 # adjust spacing between points
r <- 10 # radius
x <- 0
y <- 0
for (i in 1:n) {
  x[i] <- r*sqrt(k*i)*cos(sqrt(k*i))
  y[i] <- r*sqrt(k*i)*sin(sqrt(k*i))
  
}



z <- as.data.frame(cbind(x,y))

digits <- z %>% ggplot(aes(x ,y, color = factor(pie[,1]))) + 
  geom_text(aes(label=pie[,1], color=factor(pie[,1])), size=5) +
  coord_fixed() + theme_void() + theme(legend.position = "none", 
                                       panel.background = element_rect(fill = "black"),
                                       plot.background = element_rect(fill = "black"))


dots <- z %>% ggplot(aes(x ,y, color = factor(pie[,1]))) + 
  geom_point(aes(color=factor(pie[,1])), size=4) +
  coord_fixed() + theme_void() + theme(legend.position = "none", 
                                       panel.background = element_rect(fill = "black"),
                                       plot.background = element_rect(fill = "black"))


dots_even <- z %>% mutate(even=pie[,1] %% 2) %>% 
  ggplot(aes(x ,y, color = factor(pie[,1]))) + 
  geom_point(aes(color=even), size=4) +
  coord_fixed() + theme_void() + 
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))


ggsave(
  filename = "silk19.jpeg",
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
