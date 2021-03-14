# random walk
library(tidyverse)
library(ggplot2)

# number of points
n <- 1000

pie <- read.csv('pi.csv')

x <- 0
y <- 0

angle <- 2*pi/10
walk_x1 <- 0
walk_y1 <- 0
walk_x2 <- 0
walk_y2 <- 0

# creating column with 0 as the starting of pie series instead of 3
aa <-data.frame(0)
aa[2:1000,] <- pie[1:999,1]

# switching the newly created vector in pie variable
pie[,2] <- aa
# let's add the direction to the digits
pie <- pie %>% mutate(rot_1 = pie[,1]*angle, 
                      rot_2 = pie[,2]*angle,
                      x1 =  cos(rot_1),
                      y1 =  sin(rot_1),
                      x2 =  cos(rot_2),
                      y2 =  sin(rot_2),
                      walk_x1 = cumsum(x1),
                      walk_y1 = cumsum(y1),
                      walk_x2 = cumsum(x2)-1,
                      walk_y2 = cumsum(y2))


pie %>% ggplot(aes(walk_x2,walk_y2, col = rainbow(n))) + 
  coord_fixed() +
  geom_segment(aes(xend=walk_x1,yend=walk_y1),size=0.5) +
  geom_point(size=.5) +
  theme_void() + 
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))


ggsave(
  filename = "silk20.jpeg",
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
