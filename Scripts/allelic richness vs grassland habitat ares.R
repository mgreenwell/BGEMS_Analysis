library(tidyverse)

LCM_data <- read.csv("Data/Sites_&_GIS_Data/LCM2007_BMS_1km_buffer.csv")
species_codes <- read.csv("Data/Sites_&_GIS_Data/site_number_allocations.csv")
allelic_richness <- read.csv("Data/Labs/PopGenReport-allelic_richness.csv")

species_codes <- select(species_codes, Code, siteno.gref)

sites <- species_codes$siteno.gref

LCM_data <- filter(LCM_data, SITENO %in% sites)


LCM_data <- merge(LCM_data,species_codes,by.x = "SITENO", by.y = "siteno.gref")
LCM_data <- select(LCM_data, Code, SITENO:TOTAL.COUNT)

allelic_richness <- rename(allelic_richness, Locus = X)
allelic_richness <- gather(allelic_richness, site, allelic_richness, ARN:TC)


all_data <- merge(LCM_data, allelic_richness ,by.x = "Code", by.y = "site")
all_data
all_data <- select(all_data, Code, Broadleaved.woodland, Rough.Grassland,
                   Neutral.Grassland, Calcareous.Grassland, Locus, allelic_richness)
all_data
all_data <- all_data %>%
  mutate(good_habitat = 
           Broadleaved.woodland + 
           Rough.Grassland + 
           Neutral.Grassland + 
           Calcareous.Grassland) %>%
select(Code, good_habitat, Locus, allelic_richness)



ggplot(all_data, aes(good_habitat, allelic_richness)) +
  geom_point(aes(color = Locus)) +
  geom_line(aes(colour = Locus))
