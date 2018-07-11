# =========================== Packages required =============================== 


library(raster)
library(tidyverse)


# ============================= Load in data ==================================


# european_coordinates.csv contains all site locatiosn for BGEMS across Europe

locations <- read.csv("Data/Sites/european_coordinates.csv", header = T)


# ============================== Format data ==================================


# In locations columns are ordered latitude then longitude
# R requires columns to be the other way around i.e. long (x) then lat (y)

coordinates <- mutate(locations, longitude1 = longitude, latitude1 = latitude)


# Remove orignal lat long columns

coordinates <- subset(coordinates, select=-c(longitude, latitude))


# Rename lat1 and long1
# Remove all sites not in England
# Remove Warton Cragg site as not used in analysis

coordinates <- coordinates %>%
  rename(latitude = latitude1, longitude = longitude1) %>%
  filter (country == "England", name != "Warton Crag LNR")


# Turn coordinates into spatial points dataframe
# coordinates gets a spatial point data frame using columns 5 (long) and 6 (lat)
# of the dataframe coordinates
# proj4string = CRS(etc) tells R which coordinate reference system to use
  # Unlikely to plot without information
  # Using WGS84 CRS
  # Code to ut in CRS() available at:
  # http://spatialreference.org/ref/epsg/wgs-84/
  # http://spatialreference.org/ref/epsg/wgs-84/proj4/

coordinates <-
  SpatialPointsDataFrame(
    coordinates[, 5:6],
    coordinates,
    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  )


# Check whether working
# Plot shows locations of points

plot(coordinates)


# Create matrix of distances (meters) between pairs of sites

site_distances <-
  pointDistance(coordinates,
                coordinates,
                allpairs = T,
                lonlat = TRUE)

site_distances

# Convert into a dataframe

site_distances <- as.data.frame(site_distances)


# Add row names and column names to matrix
colnames(site_distances) <- c(as.character(coordinates$code))
rownames(site_distances) <- c(as.character(coordinates$code))


# Divide matrix by 1000 to get distance in KM instead of M

site_distances <- site_distances/ 1000


# Write matrix to a csv

#write.csv(site_distances, "Outputs/site_distance_matrix.csv")


# Convert row names into the 1st column of the dataframe

site_distances <- rownames_to_column(site_distances, var = "site_1")


# Gather dataframe together so that instead of matrix, dataframe is long list
# Reduce to three rows, site 1, site 2 and distance between sites in meters

site_distances <- site_distances %>% 
  gather(site_2, distance, LC:WW)





# Create list of site names

site_list <- unique(site_distances$site_1)


# Create empty dataframe

site_distances_arranged <- NULL


# For loop rearranges dataframe so that the distances are ordered for each site
# i.e. arranged highest to lowest distances between LC and all other sites
# followed by highest to lowest distances between AU and all other sites

for (i in site_list){
  df <- site_distances %>% 
    filter(site_1 == i) %>%
    arrange(distance)
  site_distances_arranged = rbind(site_distances_arranged, df)
}


site_distances_arranged


# Detach raster package as overwrites some dplyr commands e.g. select

detach(package:raster, unload = TRUE)