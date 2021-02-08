# ggplot2: 5 helpful features and tricks

# lets analyze sample dataset of USA crime in 50 states

# loading libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

# load dataset
data("USArrests")

# getting to know the variables
str(USArrests)

# adding column to the dataset with names of 50- states
USArrests <- cbind(States = rownames(USArrests), USArrests)
str(USArrests)

# remove the repeating column (have to find a way to do it in one go)
USArrests <- USArrests[,-2]

# tip1: use of brackets (), helps plotting in a single go
#plot <- USArrests %>% ggplot(aes(Murder)) + geom_dotplot()
plot <- USArrests %>% ggplot(aes(States, Murder)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Murders per 100,000") +
  xlab("USA States") +
  #scale_y_discrete(limits=c(0:10)) +
  theme_bw() +
  coord_flip()
plot

(plot1 <- USArrests %>% ggplot(aes(States, Murder)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Murders per 100,000") +
    #scale_y_discrete(limits=c(0:10)) +
    theme_bw() +
    coord_flip()) 


# tip 2: switiching the data without changing the plotting conditions
# issue of the variable names have to remain identical as required by the 
# ggplot aesthetics
temp <- USArrests[,c(1,3)] %>% rename(Murder = Assault)
plot2 <- plot %+% temp
plot2 + ylab("Assaults per 100,000")

# tip 3: another cool feature is col, color or colour
plot3 <- USArrests %>% ggplot(aes(States, Murder)) + 
  geom_bar(stat = "identity", col = "green") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Murders per 100,000") +
    xlab("USA States") +
    #scale_y_discrete(limits=c(0:10)) +
    theme_bw()

plot4 <- USArrests %>% ggplot(aes(States, Murder)) + 
    geom_bar(stat = "identity", color = "blue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Murders per 100,000") +
    xlab("USA States") +
    #scale_y_discrete(limits=c(0:10)) +
    theme_bw()

plot5 <- USArrests %>% ggplot(aes(States, Murder)) + 
    geom_bar(stat = "identity", colour = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Murders per 100,000") +
    xlab("USA States") +
    #scale_y_discrete(limits=c(0:10)) +
    theme_bw()

plot3 + plot4 + plot5 + plot_layout(ncol = 3)

# tip 4: reordering the bar plot for aesthetics
(plot6 <- USArrests %>% ggplot(aes(x = fct_reorder(States, Murder, .desc = FALSE), y = Murder)) + 
    geom_bar(stat = "identity", colour = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Murders per 100,000") +
    xlab("USA States") +
    #scale_y_discrete(limits=c(0:10)) +
    theme_bw() +
    coord_flip() )


# tip 5: ggsave() for prints or publications
# saves in working directory
# define size of image, resolution, extension, background etc

ggsave(
  filename = "silk17.jpeg",
  device = png(),
  path = NULL,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 600,
  limitsize = TRUE,
  bg = "transparent"
)

