# Packages ---------------------------------------------------------------------

# Require Package
library(tidyverse)

# Data -------------------------------------------------------------------------

# Read in data
# gis.data is spreadsheet of CEH landcover map with data for sites including woodland area etc.

gis.data <-
  read.table(
    "C:\\Users\\dp005352\\Dropbox\\PhD\\R_Projects\\Phenology_Analysis\\Landscape_Phenology_Analysis\\Data\\BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",
    header = T)

# Formating --------------------------------------------------------------------

# Sort data so that only includes only ukbms rows
gis.data <- gis.data[gis.data$Surv == "UKBMS", ]
  
#Add row - convert LC to percentage of LC
gis.data$LCper <-gis.data$LC / (pi * 500 ^ 2) * 100           

# Filter data so that only BGEMS sites are in dataframe
head(gis.data)
gis.data <-
  gis.data %>% filter(
    siteno.gref == 29 |
      siteno.gref == 30 |
      siteno.gref == 1312 |
      siteno.gref == 1311 |
      siteno.gref == 1309 |
      siteno.gref == 1050 |
      siteno.gref == 1318 |
      siteno.gref == 1323 |
      siteno.gref == 1314 |
      siteno.gref == 1321 |
      siteno.gref == 1310 |
      siteno.gref == 1319 |
      siteno.gref == 1024 |
      siteno.gref == 1322 |
      siteno.gref == 1111 | 
      siteno.gref == 117 | 
      siteno.gref == 994
  )

# Saving file ------------------------------------------------------------------

#Write new file to csv
write.csv(gis.data, "C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\site_gis_data.csv", row.names = F)
