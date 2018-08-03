Created by Tom Oliver
This README created 09/11/10

This files contains landscape attribute data around UKBMS (1387),BBS (4112) and CBC (1184) sites at four spatial scales 0.5, 2, 5 and 10km radius around site centroids. (26732 rows in total) From LCM 2000 map

Column ID's are as follows:

SURV: Survey type- Breeding bird survey, Common Bird Census, UK butterfly monitoring scheme
site.no.gref =  Unique Site ID number for UKBMS sites, or grid reference for BBS and CBC
buffer = radius distance of landscape buffer around site centroid (m)
POINT_X/Y = site easting and northing in km
EASTNESS MEAN: Mean Eastness of aspect in landscape around site (Eastness = sin((aspectxPI)/180), such that 1 = due East, -1 = due West). All data handling and extraction for this and next seven columns performed in ArcGIS 9.3.1 using 50m DEM map reference (Morris DG, Flavin RW, editors. A digital terrain model for hydrology. Proceedings of the 4th International Symposium on Spatial Data Handling Vol1; 1990; Zurich)
EASTNESS_STD = Standard deviation of Eastness values
SLOPE_MEAN = Mean slope (degrees from horizontal, such that 0 = flat, 90 = vertical)
SLOPE_STD = Standard deviation of slope
NORTNESS_MEAN = Mean Northness (Northness = cos((aspectxPI)/180), such that 1 = due North, -1 = due South)
NORTNESS_STD = Standard deviation of northness values
DEM_MEAN = Mean height above sea level (m)
DEM_STD = Standard deviation of height above sea level (m)
shannon.soil = shannon index of 29 HOST soil classes in landscape (website: http://www.macaulay.ac.uk/host/; reference: Boorman DB, Hollis JM, Lilly A. Hydrology of soil types: a hydrologically based classification of the soils of the United Kingdom.; 1995). HOST map is 1km resolution map with each cell allocated to a dominant soil class. Shannon index is then calculated on the proportion of total area in each of soil classes 1:29 (i.e. after excluding missing data, e.g. areas of sea). 
dom.soil.type = identity of soil class with greatest representation in landscape
joint.dom.soil.type = NA unless two soil types are equally dominant (i.e. equal largest area), then the other dominant soil type is listed here. 
A = area of arable land in landscape
BgRo = area of bareground/rock in landscape
Br = area of bracken in the landscape
BW = = area of broadleaved woodland in landscape
C = area of coastal region in landscape
CW = area of coniferous woodland in landscape
F = area of fen/bog in landscape
G = area of all grassland types in landscape
H = area of heathland in landscape
M = area of montane region in landscape
S = area of sea in landscape
R = area of river or other inland water in landscape
UG = area of urban/suburban/gardens in landscape
##
Next columns are configuration metrics calculated on the above landcover types using the Program FRAGSTATS.
e.g. NP_A = number of separate patches of arable land in the landscape buffer
e.g. ED_A = minimum nearest neighbour distance (euclidean distance) between two patches of arable land
e.g. SHP_A = mean shape index ('edgeiness') of arable patches. Calculated as total perimeter of arable patches divided by minimum possible perimeter given the area of arable land in the buffer.
##
end.lcm = This column is filled with NA values to show that it that is the end of the Landcover 2000 map biotope values (i.e. 13 LCM biotopes with areas which sum to the total area of the landscape buffer). The following columns are analyses of grassland landcover from Natural England GIS maps (http://www.gis.naturalengland.org.uk/pubs/gis/GIS_register.asp). They were analysed separately (i.e. areas cannot simply be summed to LCM biotopes areas).
LC = area of lowland calcareous grassland
LM = area of lowland meadow
NG = area of network grassland
UDG = area of 'undetermined grassland'
Next columns are configuration metrics calculated on the above landcover types using the Program FRAGSTATS.
e.g. NP_LC = number of separate patches of lowland calcareous grasslandland in the landscape buffer
e.g. ED_LC = minimum nearest neighbour distance (euclidean distance) between two patches of lowland calcareous grasslandland 
e.g. SHP_LC = mean shape index ('edgeiness') of lowland calcareous grasslandland patches. Calculated as total perimeter of lowland calcareous grassland  patches divided by minimum possible perimeter given the area of lowland calcareous grassland in the buffer.


# end
