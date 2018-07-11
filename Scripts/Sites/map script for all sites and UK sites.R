library(ggmap)
library(ggplot2)
library(mapproj)

library(tidyverse)

coords <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\european_coordinates.csv", header = T)
coords <- na.omit(coords)


Europe <- "Berlin"
myMap <- get_map(location = Europe, source = "google", maptype = "roadmap", zoom = 4)
ggmap(myMap) + geom_point(aes(x = longitude, y = latitude), data = coords,
              color="darkred", size = 2)


mylocation <- c(lon = -1, lat = 51.5)
myMap <- get_map(location = mylocation, source = "google", maptype = "terrain", zoom = 8)
ggmap(myMap) + geom_point(aes(x = longitude, y = latitude), data = coords,
                          color="darkred", size = 3)

mylocation <- c(lon = -1, lat = 51.5)
myMap <- get_map(location = mylocation, source = "google", maptype = "terrain", zoom = 8)
ggmap(myMap) + geom_point(aes(x = longitude, y = latitude, color = cat), data = coords, size = 5) + scale_shape_manual(21) + scale_color_manual(values=c("#2CA5F5", "#5514C7", "#F22734", "#FFF93D")) + theme(legend.position="none")


# UK map
mylocation <- c(lon = -1, lat = 51.55)
myMap <- get_map(location = mylocation, source = "google", maptype = "satellite", zoom = 10)

# 4 coloured points
ggmap(myMap) + geom_point(aes(x = longitude, y = latitude, color = cat), data = coords, size = 5) + scale_shape_manual(21) + scale_color_manual(values=c("#2CA5F5", "#5514C7", "#F22734", "#FFF93D")) + theme(legend.position="none")

# 2 coloured points
ggmap(myMap) + geom_point(aes(x = longitude, y = latitude, color = cat2), data = coords, size = 5) + scale_shape_manual(21) + scale_color_manual(values=c( "#5514C7", "#F22734")) + theme(legend.position="none")


ggmap(myMap) + geom_point(aes(x = longitude, y = latitude), data = coords, size = 5, col = "red") + scalebar(coords, dist = 5, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')

?scalebar
install.packages("ggsn")
library(ggsn)


