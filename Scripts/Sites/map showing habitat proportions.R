# ============================ Required packages ==============================


library(ggmap)
library(tidyverse)
library(ggrepel)
        

# =============================== Load data ===================================


# site_gis_data.csv contains all information from the CEH Land Cover Map
# Just contains BGEMS sites

gis_data <- read.csv("Data/Sites/site_gis_data.csv")


# site_number_allocations.csv contains name data and grid refs for sites

site_numbers <- read.csv("Data/Sites/site_number_allocations.csv")

# european coordinates contains all lat on long values for all BGEMS sites

coords <- read.csv("Data/Sites/european_coordinates.csv")


# ============================== Format data ==================================


# Remove unnecessary columns from site_numbers
  # Only rewuire Code and siteno.gref

site_numbers <- select(site_numbers, Code, siteno.gref)


# Merge the datasets together in order to get names for each site with LCM data

gis_data <- merge(site_numbers, gis_data, by = "siteno.gref")


# ======================= Format data for 10km buffer =========================

# Filter data so that only 10km buffer is used (buffer == 10000)
  # Create new column of bad habitat 
    # broad leaf woodland (BW) + coniferous woodland CW) + arable land (A)
  # Create new column of good habitat grass (G)

proportions_10km <- gis_data %>%
  filter(buffer == 10000) %>%
  mutate(
    bad_habitat = BW + CW + A,
    good_habitat = G,
    total_habitat = bad_habitat + good_habitat,
    prop_good = good_habitat / total_habitat * 100)  %>%
  select(Code, prop_good)


# Remove Warton Cragg and Whitecross green as not included in analysis

proportions_10km <- proportions_10km %>% 
  filter(Code != c("WC"), Code != c("WG")) %>%
  arrange(prop_good)


# ======================= Format data for 5km buffer =========================


# Filter data so that only 5km buffer is used (buffer == 5000)
# Create new column of bad habitat 
# broad leaf woodland (BW) + coniferous woodland CW) + arable land (A)
# Create new column of good habitat grass (G)

proportions_5km <- gis_data %>%
  filter(buffer == 5000) %>%
  mutate(
    bad_habitat = BW + CW + A,
    good_habitat = G,
    total_habitat = bad_habitat + good_habitat,
    prop_good = good_habitat / total_habitat * 100)  %>%
  select(Code, prop_good)


# Remove Warton Cragg and Whitecross green as not included in analysis

proportions_5km <- proportions_5km %>% 
  filter(Code != c("WC"), Code != c("WG")) %>%
  arrange(prop_good)


# ========================== Format map data ==================================


# Remove sites with NA values i.e. no coordinates

coords <- na.omit(coords)


# Remove sites not in England
# Remove Warton Cragg as not in analysis
coords <- coords %>% 
  filter(country == c("England"), 
         name != c("Warton Crag LNR"))


# ========================== Create 10km buffer map ===========================


# Create new df with cordinates and data

coords_10km <- merge(coords, proportions_10km, by.x = "code", by.y = "Code")


# Set location of centre of map

mylocation <- c(lon = -1, lat = 51.525)


# Create base map from google maps using ggmap. 

myMap10km <- get_map(location = mylocation, 
                 source = "google", 
                 maptype = "satellite", 
                 zoom = 10)

# Add points to map with geom_point
# Scale colours of points to show habitat 
# Add text labels with geom_text_repel

ggmap(myMap10km) +
  geom_point(
    aes(x = longitude,
      y = latitude,
      col = coords_10km$prop_good),
    data = coords_10km,
    size = 5) +
  scale_colour_gradient(low = "red", high = "green") +
  geom_text_repel(data = coords_10km,
    aes(x = longitude, y = latitude, label = code),
    size = 5,
    vjust = 0,
    hjust = -0.5,
    col = "white") +
  ggtitle("10km habitat buffers")


# ========================== Create 5km buffer map ===========================


# Create new df with cordinates and data

coords_5km <- merge(coords, proportions_5km, by.x = "code", by.y = "Code")


# Set location of centre of map

mylocation <- c(lon = -1, lat = 51.525)


# Create base map from google maps using ggmap. 

myMap5km <- get_map(location = mylocation, 
                     source = "google", 
                     maptype = "satellite", 
                     zoom = 10)

# Add points to map with geom_point
# Scale colours of points to show habitat 
# Add text labels with geom_text_repel

ggmap(myMap5km) +
  geom_point(
    aes(x = longitude,
        y = latitude,
        col = coords_5km$prop_good),
    data = coords_5km,
    size = 5) +
  scale_colour_gradient(low = "red", high = "green") +
  geom_text_repel(data = coords_5km,
                  aes(x = longitude, y = latitude, label = code),
                  size = 5,
                  vjust = 0,
                  hjust = -0.5,
                  col = "white") +
  ggtitle("5km habitat buffers")

