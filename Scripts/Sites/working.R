library(ggmap)
library(tidyverse)



gis_data <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\site_gis_data.csv")

site_numbers <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\site_number_allocations.csv")


site_numbers <- select(site_numbers, Code, siteno.gref)
gis_data <- merge(site_numbers, gis_data, by = "siteno.gref")


proportions <- gis_data %>% 
  filter(buffer == 10000) %>% 
  mutate(bad_habitat = BW + CW, good_habitat = G, total_habitat = bad_habitat + good_habitat, prop_good = good_habitat / total_habitat * 100)  %>%
  select(Code, prop_good)
proportions
proportions <- proportions %>% 
  filter(Code != c("WC"), Code != c("WG")) %>%
  arrange(prop_good)
proportions

#====

coords <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\european_coordinates.csv", header = T)
coords <- na.omit(coords)
coords
coords <- coords %>% filter(country == c("England"), name != c("Warton Crag LNR"))

coords <- merge(coords, proportions, by.x = "code", by.y = "Code")

# UK map
mylocation <- c(lon = -1, lat = 51.55)
myMap <- get_map(location = mylocation, source = "google", maptype = "satellite", zoom = 10)


ggmap(myMap) + 
  geom_point(aes(x = longitude, y = latitude, col = coords$prop_good), 
             data = coords, size = 5) + 
  scale_colour_gradient(low = "red", high = "green")  


# To do 
# Work out which site is missing by labeling points
