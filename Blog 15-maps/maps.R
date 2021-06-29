# loading the packages

library(ggplot2)
library(sf)
library(tidyverse)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthhires)
library(viridis)
library(ggspatial)

# set the ggplot theme to a lighter version
theme_set(theme_light())

#loading the goespatial data for the boundaries of the countries
# using the rnaturalearth package
world <- ne_countries(scale = "medium", returnclass = "sf")

# listing the column names
names(world)

# listing the data class
class(world)

# loading the map for the first time
world %>% ggplot() + geom_sf()

# playing with aesthetics
world %>% ggplot() + 
  geom_sf(color = "black", fill = "yellow") 

# playing with aesthetics
world %>% ggplot() + 
  geom_sf(color = "black", aes(fill = gdp_md_est))

# adding labels
continent_label <- world %>% 
                        group_by(continent)  %>% 
  filter(continent != "Seven seas (open ocean)") %>%
                        summarize(area = sum(pop_est))
                     

world %>% ggplot() + 
  geom_sf(aes(fill=continent), color="black") +
  geom_sf_label(aes(label=continent))

# zooming in on the South-East Asia region
world %>% ggplot() +
  geom_sf() +
  coord_sf(xlim = c(88.594382881345, 152.93774260658), 
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE)

# population estimate
world %>% ggplot() +
  geom_sf(aes(fill = pop_est), color = "Black") +
  coord_sf(xlim = c(88.594382881345, 152.93774260658), 
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE)


# zooming on the SE Asia region
world %>% 
  ggplot() +
  geom_sf(color="black", fill = ifelse(world$subregion == "South-Eastern Asia","green","red")) +
  coord_sf(xlim = c(88.594382881345, 152.93774260658), 
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE)


# floating SE Asia region
world %>% filter(subregion == "South-Eastern Asia") %>% 
  select(name,geometry,pop_est) %>%
  ggplot() +
  geom_sf(color="black", aes(fill = pop_est)) +
  coord_sf(xlim = c(88.594382881345, 152.93774260658), 
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE)

# non-floating SE Asia region
SE_Asia <- world %>% filter(subregion == "South-Eastern Asia") %>% 
  select(name,subunit,geometry,pop_est)

(SEA_map <- world %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data=SE_Asia, aes(fill=pop_est),color="black") +
  coord_sf(xlim = c(88.594382881345, 152.93774260658), 
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE))


# filling in the details on the map
# loading maps package
library(maps)

# loading the dataset
head(world.cities)

class(world.cities)

# extracting the capitals of SE Asia
capital <- world.cities %>% 
  filter(country.etc %in% SE_Asia$subunit, capital == 1)


SEA_map + 
  geom_point(data=capital, aes(long, lat), size = 2, color="red")


# convert capital to sf object

capital <- st_as_sf(capital, coords = c("long", "lat"), crs = 4326)#, agr = "constant")


(SE_Asia_map <- world %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data=SE_Asia, aes(fill=pop_est),color="black") +
  geom_sf(data=capital, size = 2, color = "red") +
  coord_sf(xlim = c(88.594382881345, 152.93774260658), 
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE) )


# adding country labels by calculating the centroids

SEA_centroid <- st_centroid(world %>% 
                              filter(subregion == "South-Eastern Asia") %>% 
                              select(name))

# cap_coord <- st_coordinates(capital)
# 
# cap_coord <- as.data.frame(cbind(capital$name, cap_coord))
# 
# SEA_centroid <- as.data.frame(cbind(SEA_centroid$name,st_coordinates(SEA_centroid)))

# final map


world %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data=SE_Asia, aes(fill=pop_est),color="black") +
  geom_sf(data=capital, size = 2, color = "red") +
  geom_sf_label(data=SEA_centroid,aes(label=name),
                    nudge_x = c(1,-5.5,2,2,-1,-2,2.5,1,-7,1,4), 
                    nudge_y = c(3.5,-8,1.5,5.5,2.5,1.5,6,-1.75,-0.5,2.5,1.5)) +
  # geom_sf_label(data=SE_Asia,aes(label=name),
  #                  nudge_x = c(1,-1.5,2,2,-1,5,2.5,1,-7,1,4), 
  #                  nudge_y = c(2.5,-4,1.5,5.5,2.5,1.5,6,-1.75,-0.5,2.5,1.5)) +
  coord_sf(xlim = c(88.594382881345, 152.93774260658),
           ylim = c(31.29186472309,-12.375471986864), expand = FALSE) +
  # adding scale and compass
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  # adding color the waters and other aesthetics
  
  theme(panel.background = element_rect(fill = "aliceblue")) + 
  xlab("") + ylab("") +
  guides(fill = guide_colourbar(label = FALSE))+
  scale_fill_viridis() +
  labs(fill = "Population") 

ggsave("map_final.png")
