# code for blog post
#"The secret to creating your own color palette for data visualization-Part 1"

#library
library(tidyverse)

# load dataset
data("mtcars")

# structure of the dataset
str(mtcars)

# converting row names to column
cars <- rownames_to_column(mtcars,var="cars")

# Implementing - Complementary color scheme
(plot1 <- cars %>%
    ggplot(aes(wt,hp)) +
    geom_point(aes(color = factor(am)), size = 4) + 
    scale_color_manual(values = c("#d12e9f","#2ed160")) +
    theme_classic() +
    xlab("Weight (1000 lbs)") +
    ylab("Gross horsepower") +
    theme(panel.border = element_rect(colour = "black", fill=NA)))
  

# Implementing - triadic
  
(plot2 <- cars %>%
  ggplot(aes(mpg,cyl)) +
  geom_boxplot(aes(fill = factor(cyl))) + 
  scale_color_manual(values = c("#EB14D7","#D7EB14", "#14D7EB"), 
                     aesthetics = c("colour", "fill")) +
  theme_classic() +
  xlab("Miles/(US) gallon") +
  ylab("# of cylinders") +
  theme(panel.border = element_rect(colour = "black", fill=NA)))

# Implementing -  tetradic
# c("#EBD613", "#E83B17", "#15EB99", "#4C1BEB")

(plot3 <- cars %>%
    ggplot(aes(carb)) +
    geom_density(aes(fill = factor(vs))) + 
    scale_color_manual(values = c("#EBD613", "#E83B17", "#15EB99", "#4C1BEB"), 
                       aesthetics = c("colour", "fill")) +
    theme_classic() +
    xlab("Miles/(US) gallon") +
    ylab("# of cylinders") +
    theme(panel.border = element_rect(colour = "black", fill=NA)))


# Code for Monochromatic color scheme


col_grad <- c("#8F69B6","#6F4995","#A789C5","#BCA5D3","#DDD1E9","#CDBBDE","#EAE3F2")

(plot5 <- cars %>% filter(str_detect(cars, "^Me")) %>%
  ggplot(aes(fct_reorder(cars, mpg, .desc = TRUE), mpg, col = cars)) +
  geom_bar(stat = "identity", aes(fill = cars)) + 
  scale_color_manual(values = col_grad, 
                     aesthetics = c("colour", "fill")) +
  theme_classic() +
  xlab("Merc models") +
  ylab("Miles/(US) gallon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none"))
