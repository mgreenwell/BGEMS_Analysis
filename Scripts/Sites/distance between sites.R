locations <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\european_coordinates.csv", header = T)




library(raster)
library(tidyverse)

coordinates <- mutate(locations, longitude1 = longitude, latitude1 = latitude)
coordinates <- subset(coordinates, select=-c(longitude, latitude))
coordinates <- coordinates %>%
  rename(latitude = latitude1, longitude = longitude1) %>%
  filter (country == "England", name != "Warton Crag LNR")


coordinates <- SpatialPointsDataFrame(coordinates[,5:6], coordinates, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(coordinates)

site_distances <- pointDistance(coordinates, coordinates, allpairs = T, lonlat=TRUE)
site_distances <- as.data.frame(site_distances)
colnames(site_distances) <- c(as.character(coordinates$code))
rownames(site_distances) <- c(as.character(coordinates$code))
site_distances
  select(site_1,everything())
head(site_distances)

site_distances <- rownames_to_column(site_distances, var = "site_1")

site_distances <- site_distances %>% 
  gather(site_2, distance, LC:WW)
site_distances
site_distances <- site_distances %>% 
  mutate(distance_km = distance / 1000) 
site_distances <- subset(site_distances, select=-c(distance))

arrange(site_distances, site_1, site_2)

site_list <- unique(site_distances$site_1)

site_distances_arranged <- NULL
is.character(site_distances$site_1)
for (i in site_list){
  df <- site_distances %>% 
    filter(site_1 == i) %>%
    arrange(distance_km)
  site_distances_arranged = rbind(site_distances_arranged, df)
}
site_distances_arranged
