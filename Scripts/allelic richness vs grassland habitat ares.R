# =============================== Overview ====================================


# Script plots allelic richness vs amount of favourabke habitat
# There is no significant association between the two variables, bith when 
# split by locus and when all data is grouped together.
# When deciduous woodland removed as favourable habitat NED 5522 shows 
# significant positive relationship between amount of good habitat and allelic
# richness


# ========================== Packages Required ================================


library(tidyverse)


# ============================== Read in Data =================================


# LCM_data contains 1km buffers around sites with areas of different habitat 
# types from the 2007 CEH Land Cover Map

LCM_data <- read.csv("Data/Sites_&_GIS_Data/LCM2007_BMS_1km_buffer.csv")


# species_codes contains all the sites in the BGEMS project and their matching
# codes in the LCM

species_codes <- read.csv("Data/Sites_&_GIS_Data/site_number_allocations.csv")


# allelic_richness contains the mean number of alleles per locus and site data

allelic_richness <- read.csv("Data/Labs/PopGenReport-allelic_richness.csv")


# =============================== Format Data =================================


# Select only the required columsn from the species_codes dataset

species_codes <- select(species_codes, Code, siteno.gref)


# Get vector of site numbers from the species codes data

sites <- species_codes$siteno.gref


# Filter the LCM_data to only contain BGEMS sites using the new sites vector

LCM_data <- filter(LCM_data, SITENO %in% sites)


# Merge LCM_data and species codes together

LCM_data <- merge(LCM_data,species_codes,by.x = "SITENO", by.y = "siteno.gref")


# Select only the required columns

LCM_data <- select(LCM_data, Code, SITENO:TOTAL.COUNT)


# Rename column 1 as allelic_richness

allelic_richness <- rename(allelic_richness, Locus = X)


# Change table format from wide to longform

allelic_richness <- gather(allelic_richness, site, allelic_richness, ARN:TC)


# merge allelic_richness to LCM_data

all_data <- merge(LCM_data, allelic_richness ,by.x = "Code", by.y = "site")


# Select only the columns code, locusm, allelic richness and all grassland types

all_data <- select(all_data, Code, Broadleaved.woodland, Rough.Grassland,
                   Neutral.Grassland, Calcareous.Grassland, Locus, allelic_richness)


# Add all grassland columns together to get a single column totaling all grasses
# Remove old grass columns

all_data <- all_data %>%
  mutate(good_habitat = 
           Broadleaved.woodland + 
           Rough.Grassland + 
           Neutral.Grassland + 
           Calcareous.Grassland) %>%
select(Code, good_habitat, Locus, allelic_richness)


# ========================== Plots and Linear Models ==========================


# Plot results showing amount of favourable habitat vs allelic richness
# All sites and loci, not split

plot1 <- ggplot(all_data, aes(good_habitat, allelic_richness)) +
  geom_point() +
  stat_smooth(method = "lm")
 plot1


# Linear model of allelic richness vs amount of favourable habitat
 
linearmod <- lm(allelic_richness ~ good_habitat, data = all_data) 

summary(linearmod)


# Plot results showing amount of favourable habitat vs allelic richness, 
# split by locus 

plot2 <- ggplot(all_data, aes(good_habitat, allelic_richness)) +
  geom_point(aes(color = Locus)) +
  geom_line(aes(colour = Locus)) 

plot2


# Plot results showing amount of favourable habitat vs allelic richness, 
# split by locus and linear models run

plot3 <- ggplot(all_data, aes(good_habitat, allelic_richness)) + 
  geom_point() +
  geom_smooth(method=lm) +
  facet_wrap(~Locus, scales="free_x") +
  labs(x = "Area of favourable habitat", y = "Allelic Richness")

plot3


# Create list of loci

loci_list <- unique(all_data$Locus)
 

# Loop runs a linear model for each locus of good habitat vs allelic richness

for( i in loci_list) {
   filtered_data <- filter(all_data, Locus == i)
   linearmod <- lm(allelic_richness ~ good_habitat, data = filtered_data) 
   print(i)
   print(summary(linearmod))
}


# ==================== Re running removing deciduous woodland ================


all_data_deciduous_removed <-
  merge(LCM_data, allelic_richness , 
        by.x = "Code", by.y = "site")


# Select only the columns code, locus, allelic richness and all grassland types

all_data_deciduous_removed <- select(
  all_data_deciduous_removed,
  Code,
  Rough.Grassland,
  Neutral.Grassland,
  Calcareous.Grassland,
  Locus,
  allelic_richness
)


# Add all grassland columns together to get a single column totaling all grasses
# Remove old grass columns

all_data_deciduous_removed <- all_data_deciduous_removed %>%
  mutate(good_habitat = 
           Rough.Grassland + 
           Neutral.Grassland + 
           Calcareous.Grassland) %>%
  select(Code, good_habitat, Locus, allelic_richness)


# ========================== Plots and Linear Models ==========================


# Plot results showing amount of favourable habitat vs allelic richness
# All sites and loci, not split

plot4 <-
  ggplot(all_data_deciduous_removed,
         aes(good_habitat, allelic_richness)) +
  geom_point() +
  stat_smooth(method = "lm")

plot4


# Linear model of allelic richness vs amount of favourable habitat

linearmod <- lm(allelic_richness ~ good_habitat, 
                data = all_data_deciduous_removed) 

summary(linearmod)


# Plot results showing amount of favourable habitat vs allelic richness, 
# split by locus 

plot5 <- ggplot(all_data_deciduous_removed, 
                aes(good_habitat, allelic_richness)) +
  geom_point(aes(color = Locus)) +
  geom_line(aes(colour = Locus)) 

plot5


# Plot results showing amount of favourable habitat vs allelic richness, 
# split by locus and linear models run

plot6 <- ggplot(all_data_deciduous_removed, 
                aes(good_habitat, allelic_richness)) + 
  geom_point() +
  geom_smooth(method=lm) +
  facet_wrap(~Locus, scales="free_x") +
  labs(x = "Area of favourable habitat", y = "Allelic Richness")

plot6


# Create list of loci

loci_list <- unique(all_data_deciduous_removed$Locus)


# Loop runs a linear model for each locus of good habitat vs allelic richness

for( i in loci_list) {
  filtered_data <- filter(all_data_deciduous_removed, Locus == i)
  linearmod <- lm(allelic_richness ~ good_habitat, data = filtered_data) 
  print(i)
  print(summary(linearmod))
}




