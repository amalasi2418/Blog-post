library(ggplot2)
library(patchwork)
##############################

# expand.grid() function to create all possible combinations between the variables H,S,L
##############################################################
HSV1 <- expand.grid(H=0, S=0, V=seq(0,1,0.001))

p1 <- ggplot() +
  facet_wrap(~S) +
  scale_y_continuous(breaks=seq(0, 1)) +
  scale_fill_identity() +
  ggtitle("Luminance") +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(HSV1, mapping=aes(xmin=H, xmax=H+resolution(H), 
                                  ymin=V, ymax=V+resolution(V), fill=hsv(H,S,V)))


HSV2 <- expand.grid(H=0, S=1, V=seq(0,1,0.001))

p2 <- ggplot() +
  facet_wrap(~S) +
  ggtitle("Saturation") +
  scale_y_continuous(breaks=seq(0, 1)) +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(HSV2, mapping=aes(xmin=H, xmax=H+resolution(H), 
                                  ymin=V, ymax=V+resolution(V), fill=hsv(H,S,V)))


HSV3 <- expand.grid(H=seq(1,0,-0.001), S=1, V=1)

p3 <- ggplot() +
  facet_wrap(~V) +
  ggtitle("Hue") +
  scale_x_continuous(limits = c(0,360), breaks=seq(0, 360, 60)) +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()) +
  coord_flip() +
  geom_rect(HSV3, mapping=aes(xmin=H, xmax=(H+resolution(H))*360, 
                                  ymin=S, ymax=S+resolution(S), fill=hsv(H,S,V)))

p3+p2+p1
######################DONE##############

##### GENERATING COLOR WHEEL ###########

# expand.grid() function to create all possible combinations between the variables H,S,L
HSV4 <- expand.grid(H = seq(0,1,.001),S = seq(0,1,.01), V = 1)

# color wheel 
p4 <- ggplot() +
  geom_rect(HSV4, mapping=aes(xmin = H, xmax = (H+resolution(H)), 
                              ymin = S, ymax = S + resolution(S), fill = hsv(H,S,V))) +
# resolution() tales the least count or incremental value of the variable.
  ggtitle("HSL color wheel") +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, .2)) +
  scale_fill_identity() +
  coord_polar(theta="x") +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank())


###########################
## Generating some colors

HSV6 <- expand.grid(H = seq(0,1,.2),S = seq(0,1,.01), V = seq(0,1,.2))

# for rectangular plots (color wheel equivalent)
p6 <- ggplot() +
  geom_rect(HSV6, mapping=aes(xmin = H, xmax = (H+resolution(H)), 
                              ymin = S, ymax = S + resolution(S), fill = hsv(H,S,V))) +
  # resolution() tales the least count or incremental value of the variable.
  #facet_wrap(~V) + # combines plots
  ggtitle("Hue") +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, .2)) +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank())


HSV7 <- expand.grid(H = seq(0,1,.2),S = seq(0,1,.01), V = seq(0,1,.2))

# for rectangular plots (color wheel equivalent)
p7 <- ggplot() +
  geom_rect(HSV7, mapping=aes(xmin = H, xmax = (H+resolution(H)), 
                              ymin = S, ymax = S + resolution(S), fill = hsv(H,S,V))) +
  # resolution() tales the least count or incremental value of the variable.
  facet_wrap(~V) + # combines plots
  ggtitle("HSL") +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, .2)) +
  scale_fill_identity() +
  theme_classic() +
  labs(caption = "Within a single image the Hue changes form left to right, Saturation varies from bottom to top, and Luminance 
  changes on going from top-left to bottom-right image.") +
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank(),
        plot.caption = element_text(hjust = 0, face = "italic"))

