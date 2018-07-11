library(tidyverse)
gis_data <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\site_gis_data.csv")

site_numbers <- read.csv("C:\\Users\\dp005352\\Dropbox\\PhD\\BGEMS\\site_locations\\site_number_allocations.csv")


site_numbers <- select(site_numbers, Code, siteno.gref)
gis_data <- merge(site_numbers, gis_data, by = "siteno.gref")

# five_km <- gis_data %>%
#   filter(buffer == 5000) %>%
#   mutate(W = BW + CW) %>%
#   select(Site.Name, A, W, G)
 
ten_km <- gis_data %>% 
  filter(buffer == 10000) %>% 
  mutate(W = BW + CW) %>%
  mutate(connectivity = G - A - W) %>%
  select(Site.Name, connectivity) %>%
  arrange(connectivity)
 
ten_km


proportions <- gis_data %>% 
  filter(buffer == 10000) %>% 
  mutate(bad_habitat = BW + CW, good_habitat = G, total_habitat = bad_habitat + good_habitat, prop_good = good_habitat / total_habitat * 100)  %>%
  select(Code, prop_good)
proportions
proportions <- proportions %>% 
  filter(Code != c("WC"), Code != c("WG")) %>%
  arrange(prop_good)
proportions
